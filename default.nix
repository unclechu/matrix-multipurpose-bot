let
  sources = import nix/sources.nix;
  availableBuildTools = [ "cabal" "stack" ];
in

{ pkgs ? import sources.nixpkgs {}
, inNixShell ? false
, withHLS ? true
, buildTools ? [ "cabal" ] # See “availableBuildTools”
}:

assert builtins.all (x: builtins.elem x availableBuildTools) buildTools;

let
  # +1 and -1 for the slash after the prefix
  getRelativeFileName = prefix: fileName:
    builtins.substring
      (builtins.stringLength prefix + 1)
      (builtins.stringLength fileName - builtins.stringLength prefix - 1)
      fileName;

  srcFilter = prefix: fileName: fileType:
    let relativeFileName = getRelativeFileName prefix fileName; in
    (
      (fileType == "regular") &&
      (builtins.elem relativeFileName ["LICENSE" "README.md"])
    )
    ||
    (
      (fileType == "regular") &&
      (builtins.match "^.+\.cabal$" relativeFileName != null)
    )
    ||
    (
      (fileType == "directory") &&
      (builtins.match "^(app|src)(/.+)?$" relativeFileName != null)
    )
    ||
    (
      (fileType == "regular") &&
      (builtins.match "^(app|src)/.+\.(hs)$" relativeFileName != null)
    );

  cleanSrc = path:
    assert builtins.isPath path;
    pkgs.nix-gitignore.gitignoreFilterRecursiveSource
      (srcFilter (toString path))
      [ ./.gitignore ]
      path;

  hsPkgs = pkgs.haskellPackages.extend (self: super: {
    matrix-bot =
      let
        pkg = super.callCabal2nix "matrix-bot" (cleanSrc ./.) {};
      in
        pkg // { exe = pkgs.haskell.lib.justStaticExecutables pkg; };
  });

  shell = hsPkgs.shellFor {
    packages = p: [ p.matrix-bot ];
    withHoogle = true;

    buildInputs = [
      pkgs.jq
    ] ++ pkgs.lib.optional withHLS hsPkgs.haskell-language-server
      ++ pkgs.lib.optional (builtins.elem "cabal" buildTools) hsPkgs.cabal-install
      ++ pkgs.lib.optional (builtins.elem "stack" buildTools) hsPkgs.stack;
  };
in

(if inNixShell then shell else {}) // {
  inherit (hsPkgs) matrix-bot;
  inherit shell;
}
