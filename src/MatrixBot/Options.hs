{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Application’s command-line interface
module MatrixBot.Options where

import Data.Text (Text, pack)
import Data.UUID (fromString)
import qualified Data.Attoparsec.Text as AP

import Control.Monad.IO.Class

import Options.Applicative

import MatrixBot.SharedTypes


-- * Commands, options, and their specs

data AppCommand
  = AppCommandAuth AuthOptions
  | AppCommandStart StartOptions
  | AppCommandSendMessage SendMessageOptions

appCommandParserInfo ∷ ParserInfo AppCommand
appCommandParserInfo
  = info (helper <*> appCommandParser)
  $ fullDesc <> header "matrix-bot - Matrix multipurpose bot"

appCommandParser ∷ Parser AppCommand
appCommandParser = go where
  go = hsubparser $ authCommand <> startCommand <> sendMessageCommand

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

  sendMessageCommand ∷ Mod CommandFields AppCommand
  sendMessageCommand
    = command "send-message"
    $ info
        (AppCommandSendMessage <$> sendMessageOptionsParser)
        (progDesc "Send text message to a room\
                  \ (will write a JSON object to stdout with transaction ID and server response)")


-- * Authorization

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
    , metavar "FILE"
    ]

  outputFile = strOption $ mconcat
    [ long "output"
    , short 'o'
    , help "Where to save JSON with obtained credentials to"
    , metavar "FILE"
    , value "/dev/stdout"
    , showDefault
    ]


-- * Bot start

data StartOptions = StartOptions
  { startOptionsCredentialsFile ∷ FilePath
  -- ^ Path to the file with credentials obtained via calling “auth” command
  , startOptionsBotConfigFile ∷ FilePath
  -- ^ Bot configuration JSON file
  , startOptionsRetryLimit ∷ RetryLimit
  -- ^ In case of Matrix API request exception how many times to retry before bot dies
  , startOptionsRetryDelay ∷ RetryDelay
  -- ^ In case of Matrix API request exception how long to wait before each retry
  , startOptionsEventTokenFile ∷ Maybe FilePath
  -- ^ Path to a JSON file that contains last event token to start listening events from (when
  --   application starts it reads from this file, when it receives new events it writes to it)
  }
  deriving stock (Eq, Show)

startOptionsParser ∷ Parser StartOptions
startOptionsParser = go where
  go = StartOptions
    <$> credentialsFile
    <*> botConfigFile
    <*> retryLimit'
    <*> retryDelay'
    <*> eventToken

  credentialsFile = strOption $ mconcat
    [ long "credentials"
    , short 'a'
    , help "Credentials JSON file for bot authentication (call “auth” command to get one)"
    , metavar "FILE"
    ]

  botConfigFile = strOption $ mconcat
    [ long "bot-config"
    , short 'c'
    , help "Bot configuration JSON file for bot authentication"
    , metavar "FILE"
    ]

  retryLimit' = fmap RetryLimit . option auto $ mconcat
    [ long "retry-limit"
    , help "How many times to retry failed Matrix API call before bot dies"
    , metavar "AMOUNT"
    , value 10
    , showDefault
    ]

  retryDelay' = option (RetryDelay <$> fractionalSeconds) $ mconcat
    [ long "retry-delay"
    , help "How long to wait before each retry for failed Matrix API call"
    , metavar "SECONDS"
    , value . RetryDelay . secondsToMicroseconds . Seconds $ 30
    , showDefaultWith printRetryDelaySeconds
    ]
    where
      fractionalSeconds ∷ ReadM Microseconds
      fractionalSeconds = Microseconds . round . (* 1_000_000) <$> auto @Double

  eventToken = option (Just <$> str) $ mconcat
    [ long "event-token"
    , short 'e'
    , help $ unwords
        [ "Optional JSON file to read last event token from to start listening for new events from"
        , "and where to save last event token to"
        , "(useful to not loose events from the time when the bot was offline)"
        ]
    , metavar "FILE"
    , value Nothing
    ]


-- * Send message command options

data SendMessageOptions = SendMessageOptions
  { sendMessageOptionsCredentialsFile ∷ FilePath
  , sendMessageOptionsRoomId ∷ RoomId
  , sendMessageOptionsMessage ∷ Either Text FilePath
  -- ^ Either as plan value or a file to read it from (e.g. /dev/stdin)
  , sendMessageOptionsTransactionId ∷ Maybe TransactionId
  }
  deriving stock (Eq, Show)

sendMessageOptionsParser ∷ Parser SendMessageOptions
sendMessageOptionsParser = go where
  go = SendMessageOptions
    <$> credentialsFile
    <*> roomId
    <*> message
    <*> transactionId

  credentialsFile = strOption $ mconcat
    [ long "credentials"
    , short 'a'
    , help "Credentials JSON file for authentication (call “auth” command to get one)"
    , metavar "FILE"
    ]

  roomId = option (eitherReader $ AP.parseOnly roomIdParser . pack) $ mconcat
    [ long "room-id"
    , short 'r'
    , help "Room identifier (e.g. !ffffffffffffffffff:matrix.org) where to send text message to"
    , metavar "ROOM_ID"
    ]

  message = fmap Left messageValue <|> fmap Right messageFile

  messageValue = strOption $ mconcat
    [ long "message"
    , short 'm'
    , help "Text message to send to the room"
    , metavar "TEXT"
    ]

  messageFile = strOption $ mconcat
    [ long "message-file"
    , short 'f'
    , help "Path to a file to read text message from to send it to the room (e.g. /dev/stdin)"
    , value "/dev/stdin"
    , showDefault
    , metavar "FILE"
    ]

  transactionId = option (Just . TransactionId <$> maybeReader fromString) $ mconcat
    [ long "transaction-id"
    , short 't'
    , help $ unwords
        [ "Transaction ID (any random UUID) for atomicity of the request"
        , "(if not provided new random one will be generated)"
        ]
    , metavar "UUID"
    , value Nothing
    ]


-- * Parsing command-line arguments

parseAppCommand ∷ MonadIO m ⇒ m AppCommand
parseAppCommand = liftIO $ execParser appCommandParserInfo
