{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ConstraintKinds #-}

module MatrixBot.Bot.BotM
  ( BotM
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Control.Exception.Safe as E
import qualified Control.Monad.Logger as ML
import qualified Control.Monad.Reader as MR
import qualified MatrixBot.Auth as Auth
import qualified MatrixBot.SharedTypes as T


-- | Main Bot Monad Constraint type
type BotM r m =
  ( MonadIO m
  , MonadUnliftIO m
  , MonadFail m
  , E.MonadMask m
  , MR.MonadReader r m
  , ML.MonadLogger m
  , Auth.HasCredentials r
  , T.HasRetryParams r
  )
