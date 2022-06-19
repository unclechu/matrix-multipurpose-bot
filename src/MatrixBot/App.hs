{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Application startup module
module MatrixBot.App
     ( runApp
     ) where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON (..), FromJSON (..), eitherDecodeFileStrict)
import Data.Aeson.Text (encodeToLazyText)
import Data.String (IsString)
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import qualified Data.Text.IO as TextIO

import Control.Lens.Lens (lens)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Control.Exception.Safe as E
import qualified Control.Monad.Logger as ML
import qualified Control.Monad.Reader as MR

import System.Exit (ExitCode (..))
import System.IO

import MatrixBot.AesonUtils (myGenericToJSON, myGenericParseJSON)
import MatrixBot.Log
import MatrixBot.MatrixApi (EventResponse)
import qualified MatrixBot.Auth as Auth
import qualified MatrixBot.Bot as Bot
import qualified MatrixBot.Options as O
import qualified MatrixBot.SharedTypes as T


type AppM m =
  ( MonadIO m
  , MonadUnliftIO m
  , MonadFail m
  , E.MonadMask m
  )


runApp ∷ AppM m ⇒ m ()
runApp = go where
  go = withLogger . MR.runReaderT $ E.catch startApp exceptionHandler

  exceptionHandler ∷ (MonadIO m, E.MonadThrow m, ML.MonadLogger m) ⇒ E.SomeException → m ()
  exceptionHandler e = do
    case E.fromException @ExitCode e of
      Just ExitSuccess → do
        logDebug . pack . ("Application exits with: " <>) . E.displayException $ e
        E.throwM e
      _ → do
        logError . pack . ("Application failed with: " <>) . E.displayException $ e
        E.throwM e

  startApp ∷ (AppM m, ML.MonadLogger m) ⇒ m ()
  startApp = do
    logDebug "Starting the application…"

    logDebug "Parsing the command-line arguments…"
    O.parseAppCommand >>= \case
      O.AppCommandAuth opts → runAuth opts
      O.AppCommandStart opts → runStart opts
      O.AppCommandSendMessage opts → runSendMessage opts


runAuth ∷ (MonadIO m, MonadFail m, E.MonadThrow m, ML.MonadLogger m) ⇒ O.AuthOptions → m ()
runAuth opts = do
  logDebug "Running authentication…"
  let quotedMxid = quoted . T.printMxid . O.authOptionsMxid $ opts
  logDebug $ "MXID: " <> quotedMxid

  password ←
    case O.authOptionsPassword opts of
      Left x →
        x <$ logDebug ("Password for " <> quotedMxid <> " was provided as an option argument")
      Right file → do
        logDebug $ "Reading password for " <> quotedMxid <> " from " <> quoted file <> "…"
        handle ← liftIO $ openFile file ReadMode
        liftIO $ T.Password <$> TextIO.hGetLine handle

  credentials ← Auth.authenticate (O.authOptionsMxid opts) password
  logDebug "Received credentials"

  logDebug $ "Saving credentials to " <> (quoted . O.authOptionsOutputFile) opts <> "…"
  liftIO . TextIO.writeFile (O.authOptionsOutputFile opts) . toStrict . encodeToLazyText $ credentials

  logDebug "Success!"


runStart
  ∷ (MonadIO m, MonadFail m, E.MonadMask m, MonadUnliftIO m, ML.MonadLogger m)
  ⇒ O.StartOptions
  → m ()
runStart opts = do
  logDebug "Initializing the bot…"

  let
    retryLimit = O.startOptionsRetryLimit opts
    retryDelay = O.startOptionsRetryDelay opts
    eventTokenFile = O.startOptionsEventTokenFile opts

  logDebug $ mconcat
    [ "Failed Matrix API call retry limit: "
    , pack . show . T.unRetryLimit $ retryLimit
    , " (amount of retries before bot fails completely)"
    ]

  logDebug $ mconcat
    [ "Failed Matrix API call retry interval: "
    , T.printRetryDelaySeconds retryDelay
    ]

  case eventTokenFile of
    Nothing →
      logWarn $ mconcat
        [ "There’s no event token file provided, "
        , "will start listening from next following events"
        ]
    Just x →
      logDebug $ mconcat
        [ "Event token file where to read from and save to last event token that is used "
        , "as a starting point to get next events from: ", pack . show $ x
        ]

  logDebug $ mconcat
    [ "Reading and parsing credentials "
    , quoted . O.startOptionsCredentialsFile $ opts
    , " file…"
    ]

  credentials ←
    either fail pure =<< liftIO (eitherDecodeFileStrict . O.startOptionsCredentialsFile $ opts)

  let
    quotedMxid
      = quoted . T.printMxid
      $ T.Mxid (Auth.credentialsUsername credentials) (Auth.credentialsHomeServer credentials)

  logDebug $ "MXID: " <> quotedMxid

  logDebug $ mconcat
    [ "Reading and parsing bot configuration from "
    , quoted . O.startOptionsBotConfigFile $ opts
    , " file…"
    ]

  botConfig ←
    either fail pure =<< liftIO (eitherDecodeFileStrict . O.startOptionsBotConfigFile $ opts)

  Bot.startTheBot eventTokenFile botConfig `MR.runReaderT` BotEnv credentials retryLimit retryDelay


runSendMessage
  ∷ (MonadIO m, MonadFail m, E.MonadThrow m, ML.MonadLogger m)
  ⇒ O.SendMessageOptions
  → m ()
runSendMessage opts = do
  logDebug "Running send message command…"

  let roomId = O.sendMessageOptionsRoomId opts

  credentials ∷ Auth.Credentials ← do
    logDebug $ mconcat
      [ "Reading and parsing credentials "
      , quoted . O.sendMessageOptionsCredentialsFile $ opts
      , " file…"
      ]

    either fail pure =<< liftIO (eitherDecodeFileStrict . O.sendMessageOptionsCredentialsFile $ opts)

  message ←
    case O.sendMessageOptionsMessage opts of
      Left x →
        (x <$) . logDebug $ mconcat
          [ "Message to send to ", pack . show . T.printRoomId $ roomId
          , " room was provided as an option argument"
          ]
      Right file → do
        logDebug $ mconcat
          [ "Reading message to send to ", pack . show . T.printRoomId $ roomId
          , " room from ", pack . show $ file, " file…"
          ]
        liftIO $ TextIO.readFile file

  flip MR.runReaderT credentials $
    Bot.withReqAndAuth $ \req auth → do
      transactionId ←
        case O.sendMessageOptionsTransactionId opts of
          Nothing → do
            txid ← T.genTransactionId
            (txid <$) . logDebug $ mconcat
              [ "No transaction ID was provided in the command-line options, generated new one: "
              , pack . show . T.unTransactionId $ txid
              ]
          Just txid → do
            (txid <$) . logDebug $ mconcat
              [ "Using transaction ID provided in the command-line options: "
              , pack . show . T.unTransactionId $ txid
              ]

      response ← Bot.sendMessage req auth transactionId roomId message

      logDebug "Printing response and transaction ID to stdout…"

      liftIO . TextIO.putStrLn . toStrict . encodeToLazyText $ SendMessageResponse
        { sendMessageResponseTransactionId = transactionId
        , sendMessageResponseResponse = response
        }

      logDebug "Success!"


-- * Types

data BotEnv = BotEnv
  { botEnvCredentials ∷ Auth.Credentials
  , botEnvRetryLimit ∷ T.RetryLimit
  , botEnvRetryDelay ∷ T.RetryDelay
  }
  deriving stock (Eq, Show)

instance Auth.HasCredentials BotEnv where
  credentials = lens botEnvCredentials $ \x v → x { botEnvCredentials = v }

instance T.HasRetryParams BotEnv where
  retryLimit = lens botEnvRetryLimit $ \x v → x { botEnvRetryLimit = v }
  retryDelay = lens botEnvRetryDelay $ \x v → x { botEnvRetryDelay = v }


data SendMessageResponse = SendMessageResponse
  { sendMessageResponseTransactionId ∷ T.TransactionId
  , sendMessageResponseResponse ∷ EventResponse
  }
  deriving stock (Generic, Eq, Show)

instance ToJSON SendMessageResponse where toJSON = myGenericToJSON
instance FromJSON SendMessageResponse where parseJSON = myGenericParseJSON


-- * Helpers

-- | Wrap a string into quotes
quoted ∷ (IsString s, Show s) ⇒ s → Text
quoted = pack . show
