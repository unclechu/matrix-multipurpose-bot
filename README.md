# Matrix Bot

[Matrix](https://matrix.org) multipurpose bot.

**Work in progress!** It’s in development but it’s supposed to work.

## What it can do?

- [x] Leave multiple reactions under user messages
      (see `react_to_users` section in [example bot config])
  - [x] Filter users by their MXIDs (equality check, see `users_filter`)
  - [x] Filter events by room IDs (equality check, see `rooms_filter`)
- [x] Reply with a message to media events (uploaded files)
      (see `reply_to_media` section in [example bot config])
      which can be useful for posting direct HTTP links to the files
      in case room history permissions are allowing such direct access
  - [x] Filter users by their MXIDs (equality check, see `users_filter`)
  - [x] Filter events by room IDs (equality check, see `rooms_filter`)
  - [x] Filter events by `msgtype` (equality check, see `msgtype_filter`)
  - [x] Message is generated using a template allowing dynamic values
        substitution (see `message_template`)
  - [x] HTML-formatted message template in addition to the plain text one
        (see `html_message_template`)

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

#### Running on Android

This application was tested to work on Android when built using Nix (Nix-on-Droid).
Just run `nix-build` as shown above and it supposed to work as usual.

- https://f-droid.org/en/packages/com.termux.nix/
- https://github.com/t184256/nix-on-droid

### Stack

``` sh
stack build
stack run matrix-bot -- --help
```

You can use [Stack](https://haskellstack.org/) with
[Nix configuration of this project](default.nix) as well:

``` sh
nix-shell --arg buildTools '["stack"]'
stack build
stack run matrix-bot -- --help
```

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

2. Copy-paste the [example bot config] to `bot-config.json` for instance
   and configure the bot properly in that `bot-config.json` file.

3. Start the bot using credentials file and bot config file:

   ``` sh
   matrix-bot start --credentials auth.json --bot-config bot-config.json
   ```

## Usage help

You can call `matrix-bot --help` to get usage info but it only shows the first level of help.
It shows you information about basic commands (`auth`, `start`, etc). These commands in turn have
their own help information you can reach by calling `--help` after the command name. Example:

``` sh
matrix-bot auth --help
matrix-bot start --help
```

## License

[GNU/GPLv3](LICENSE)

## Author

Viacheslav Lotsmanov

[example bot config]: bot-config-example.json
