# Matrix Bot

Matrix multipurpose bot.

**Work in progress!** It’s in development but it’s supposed to work.

## What it can do?

- [x] Leave multiple reactions under user messages
  - [x] Filter users by their MXIDs (equality check)
  - [x] Filter events by room IDs (equality check)

## How to build & run

### Nix

Nix is a recommended way to build the project.

Install Nix first: https://nixos.org/download.html#nix-install-linux

``` sh
nix-build -A matrix-bot.exe -o result-matrix-bot
result-matrix-bot/bin/matrix-bot --help
```

Or you can build it using `cabal` inside nix-shell:

``` sh
nix-shell
cabal build
cabal run matrix-bot -- --help
```

This should print you the usage info.

### Stack

**TODO:** Support build using Stack.

### Cabal

Technically you can build the project using just Cabal but there are no version bounds set for
dependencies in the `*.cabal` file. So it’s not guaranteed that next release of some dependency it
would still build.

You can check for in [nixpkgs pin](nix/sources.json) and find what versions you are supposed to use.

## Usage example

1. Authenticate first, save the authentication credentials to a file:

   ``` sh
   pass show my-matrix-password | matrix-bot auth -u @username:matrix.org | jq > auth.json
   ```

   N.B. Password is read from stdin by default. In the example above `pass` utility used to pass the
   password to stdin of `matrix-bot`. `jq` is used to prettify the credentials JSON.

2. Copy-paste the [example bot config](bot-config-example.json) to `bot-config.json` for instance
   and configure the bot properly in that `bot-config.json` file.

3. Start the bot using credentials file and bot config file:

   ``` sh
   matrix-bot start --credentials auth.json --bot-config bot-config.json
   ```

## License

[GNU/GPLv3](LICENSE)

## Author

Viacheslav Lotsmanov
