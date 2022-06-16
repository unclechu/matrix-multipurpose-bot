{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Application’s command-line interface
module MatrixBot.Options where

import Data.Text (pack)
import qualified Data.Attoparsec.Text as AP

import Control.Monad.IO.Class

import Options.Applicative

import MatrixBot.SharedTypes


-- * Commands, options, and their specs

data AppCommand
  = AppCommandAuth AuthOptions
  | AppCommandStart StartOptions

appCommandParserInfo ∷ ParserInfo AppCommand
appCommandParserInfo
  = info (helper <*> appCommandParser)
  $ fullDesc <> header "matrix-bot - Matrix multipurpose bot"

appCommandParser ∷ Parser AppCommand
appCommandParser = go where
  go = hsubparser $ authCommand <> startCommand

  authCommand ∷ Mod CommandFields AppCommand
  authCommand
    = command "auth"
    $ info
        (AppCommandAuth <$> authOptionsParser)
        (progDesc
          "Authenticate using a Matrix account and get a JSON with credentials\
          \ you can use for the bot")

  startCommand ∷ Mod CommandFields AppCommand
  startCommand
    = command "start"
    $ info
        (AppCommandStart <$> startOptionsParser)
        (progDesc "Start the bot daemon")


data AuthOptions = AuthOptions
  { authOptionsMxid ∷ Mxid
  -- ^ Where to save credentials JSON output to (can be @/dev/stdout@)
  , authOptionsPassword ∷ Either Password FilePath
  -- ^ Either file to read the password from (can be @/dev/stdin@) or the password as-is
  , authOptionsOutputFile ∷ FilePath
  }
  deriving stock (Eq, Show)

authOptionsParser ∷ Parser AuthOptions
authOptionsParser = go where
  go = AuthOptions <$> mxid <*> password <*> outputFile

  mxid = option (eitherReader $ AP.parseOnly mxidParser . pack) $ mconcat
    [ long "mxid"
    , short 'u'
    , help "MXID (in format @username:homeserver)"
    , metavar "MXID"
    ]

  password = fmap Left passwordValue <|> fmap Right passwordFile

  passwordValue = fmap Password . strOption $ mconcat
    [ long "password"
    , short 'p'
    , help "Password value (--password-file is more preferable)"
    , metavar "PASSWORD"
    ]

  passwordFile = strOption $ mconcat
    [ long "password-file"
    , short 'f'
    , help "Path to a file containing password (e.g. /dev/stdin)"
    , value "/dev/stdin"
    , showDefault
    ]

  outputFile = strOption $ mconcat
    [ long "output"
    , short 'o'
    , help "Where to save JSON with obtained credentials to"
    , metavar "FILE"
    , value "/dev/stdout"
    , showDefault
    ]


data StartOptions = StartOptions
  { startOptionsCredentialsFile ∷ FilePath
  -- ^ Path to the file with credentials obtained via calling “auth” command
  , startOptionsBotConfigFile ∷ FilePath
  -- ^ Bot configuration JSON file
  }
  deriving stock (Eq, Show)

startOptionsParser ∷ Parser StartOptions
startOptionsParser = go where
  go = StartOptions <$> credentialsFile <*> botConfigFile

  credentialsFile = strOption $ mconcat
    [ long "credentials"
    , short 'a'
    , help "Credentials JSON file for bot authentication (call 'auth' command to get one)"
    , metavar "FILE"
    ]

  botConfigFile = strOption $ mconcat
    [ long "bot-config"
    , short 'c'
    , help "Bot configuration JSON file for bot authentication"
    , metavar "FILE"
    ]


-- * Parsing command-line arguments

parseAppCommand ∷ MonadIO m ⇒ m AppCommand
parseAppCommand = liftIO $ execParser appCommandParserInfo
