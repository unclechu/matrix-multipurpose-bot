{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Helpers for the log messages
module MatrixBot.Log
     ( withLogger
     , logDebug
     , logWarn
     , logError
     , Logger (..)
     , HasLogger (..)
     , defaultMonadLoggerLog
     ) where

import GHC.Stack (HasCallStack, withFrozenCallStack, callStack)
import Prelude hiding (map)

import Data.Char (toUpper)
import Data.Text (Text, pack, map)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock
import Data.Time.Format.ISO8601 (iso8601Show)

import Control.Concurrent (myThreadId)
import Control.Lens (Lens', view)
import Control.Monad.IO.Class
import qualified Control.Monad.Logger as ML
import qualified Control.Monad.Reader as MR

import qualified UnliftIO as UIO

import qualified System.Log.FastLogger as FL


withLogger ∷ UIO.MonadUnliftIO m ⇒ (Logger → m a) → m a
withLogger m =
  UIO.withRunInIO $ \runInIO →
    FL.withFastLogger (FL.LogStderr 0) $ \f →
      let
        loggerF location logLevel msg = liftIO $ do
          t ← getTimeForLog
          tid ← pack . show <$> liftIO myThreadId
          f . FL.toLogStr $ foldMap (("[" <>) . (<> "]")) [t, lvl, loc, tid] <> " " <> msg <> "\n"
          where
            lvl = case logLevel of
              ML.LevelDebug → "DEBUG"
              ML.LevelInfo → "INFO"
              ML.LevelWarn → "WARNING"
              ML.LevelError → "ERROR"
              ML.LevelOther x → map toUpper x

            loc = mconcat
              [ pack . ML.loc_module $ location
              , ":"
              , pack . show . fst . ML.loc_start $ location
              ]
      in
        runInIO . m $ Logger loggerF


logDebug :: (ML.MonadLogger m, HasCallStack) => Text -> m ()
logDebug = withFrozenCallStack (ML.logDebugCS callStack)

logWarn :: (ML.MonadLogger m, HasCallStack) => Text -> m ()
logWarn = withFrozenCallStack (ML.logWarnCS callStack)

logError :: (ML.MonadLogger m, HasCallStack) => Text -> m ()
logError = withFrozenCallStack (ML.logErrorCS callStack)


-- * Types

newtype Logger = Logger { runLogger ∷ ∀m. MonadIO m ⇒ ML.Loc → ML.LogLevel → Text → m () }

instance {-# OVERLAPS #-} MonadIO m ⇒ ML.MonadLogger (MR.ReaderT Logger m) where
  monadLoggerLog = defaultMonadLoggerLog


class HasLogger r where
  logger ∷ Lens' r Logger

instance HasLogger Logger where
  logger = id


-- | Default implementation for "ML.MonadLogger"
defaultMonadLoggerLog
  ∷ (MonadIO m, HasLogger r, MR.MonadReader r m, ML.ToLogStr msg)
  ⇒ ML.Loc
  → ML.LogSource
  → ML.LogLevel
  → msg
  → m ()
defaultMonadLoggerLog loc _src lvl msg =
  MR.asks (view logger) >>= \x → runLogger x loc lvl . logStrToText $ msg


-- * Helpers

getTimeForLog ∷ MonadIO m ⇒ m Text
getTimeForLog = pack . iso8601Show <$> liftIO getCurrentTime


logStrToText ∷ ML.ToLogStr msg ⇒ msg → Text
logStrToText = decodeUtf8 . ML.fromLogStr . ML.toLogStr
