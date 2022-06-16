let sources = import nix/sources.nix; in

{ pkgs ? import sources.nixpkgs {}
, inNixShell ? false
, withHLS ? true
}:

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
      hsPkgs.cabal-install
      pkgs.jq
    ] ++ pkgs.lib.optional withHLS hsPkgs.haskell-language-server;
  };
in

(if inNixShell then shell else {}) // {
  inherit (hsPkgs) matrix-bot;
  inherit shell;
}
