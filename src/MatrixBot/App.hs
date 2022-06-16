{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Application startup module
module MatrixBot.App
     ( runApp
     ) where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import qualified Data.Text.IO as TextIO

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Control.Exception.Safe as E
import qualified Control.Monad.Logger as ML
import qualified Control.Monad.Reader as MR

import System.Exit (ExitCode (..), exitWith)
import System.IO

import MatrixBot.Log
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

  exceptionHandler ∷ (MonadIO m, ML.MonadLogger m) ⇒ E.SomeException → m ()
  exceptionHandler e = do
    logError . pack . ("Application failed with: " <>) . E.displayException $ e
    liftIO . exitWith $ ExitFailure 1

  startApp ∷ (AppM m, ML.MonadLogger m) ⇒ m ()
  startApp = do
    logDebug "Starting the application…"

    logDebug "Parsing the command-line arguments…"
    O.parseAppCommand >>= \case
      O.AppCommandAuth opts → runAuth opts
      O.AppCommandStart opts → runStart opts


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

  logDebug $
    "Reading and parsing credentials " <> (quoted . O.startOptionsCredentialsFile) opts <> " file…"

  credentials ←
    either fail pure =<< liftIO (eitherDecodeFileStrict . O.startOptionsCredentialsFile $ opts)

  let
    quotedMxid
      = quoted . T.printMxid
      $ T.Mxid (Auth.credentialsUsername credentials) (Auth.credentialsHomeServer credentials)

  logDebug $ "MXID: " <> quotedMxid

  logDebug $
    "Reading and parsing bot configuration from " <> (quoted . O.startOptionsBotConfigFile) opts
    <> " file…"

  botConfig ←
    either fail pure =<< liftIO (eitherDecodeFileStrict . O.startOptionsBotConfigFile $ opts)

  Bot.startTheBot botConfig `MR.runReaderT` credentials


-- * Helpers

quoted ∷ Show a ⇒ a → Text
quoted = pack . show
