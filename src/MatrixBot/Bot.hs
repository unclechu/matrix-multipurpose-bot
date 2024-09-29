{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module MatrixBot.Bot
  ( startTheBot
  , withReqAndAuth
  ) where

import Data.Functor
import Data.Text (pack)
import Control.Monad ()
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Control.Exception.Safe as E
import qualified Control.Lens as L
import qualified Control.Monad.Logger as ML
import qualified Control.Monad.Reader as MR
import qualified UnliftIO.Async as Async
import qualified UnliftIO.Concurrent as Async
import Servant.API (AuthProtect)
import Servant.Client.Core (AuthenticatedRequest)
import qualified MatrixBot.Log as L
import qualified MatrixBot.Auth as Auth
import qualified MatrixBot.MatrixApi.Client as Api
import qualified MatrixBot.SharedTypes as T
import qualified MatrixBot.Bot.BotConfig as BotConfig
import MatrixBot.Bot.BotM (BotM)
import MatrixBot.Bot.SmokeTest (startupSmokeTest)
import MatrixBot.Bot.Jobs.Handlers.MainHandler (jobsHandler)
import MatrixBot.Bot.EventsListener (eventsListener)
import MatrixBot.Bot.Jobs.Queue (HasBotJobsReader, HasBotJobsWriter)


startTheBot ∷ (BotM r m, HasBotJobsReader r, HasBotJobsWriter r) ⇒ Maybe FilePath → T.EventsTimeout → BotConfig.BotConfig → m ()
startTheBot eventTokenFile eventsTimeout botConfig = do
  L.logDebug "Starting the bot…"
  L.logDebug $ "Bot configuration: " <> (pack . show) botConfig

  withReqAndAuth eventsTimeout $ \req auth → do
    startupSmokeTest req auth

    L.logDebug "Running bot threads (jobs queue handler and room events listener)…"

    let
      jobsHandler' = jobsHandler req auth
      eventsListener' = eventsListener eventTokenFile eventsTimeout botConfig req auth

    -- Start 2 threads in parallel.
    -- One would read new Matrix room events and react to them if necessary
    -- (as defined in the Bot config, following all the filters) by adding
    -- new Bot Jobs. Meanwhile Bot Jobs handler will perform actions by hanlding
    -- the jobs from the queue.
    void . Async.runConcurrently
      $ Async.Concurrently (threadWrap jobsHandler')
      <* Async.Concurrently (threadWrap eventsListener')

  where
    logThreadFailure =
      flip E.withException $ \(e ∷ E.SomeException) →
        L.logError $ "The thread has failed with: " <> (pack . E.displayException) e

    threadWrap m = logThreadFailure $ m >>= \() → do
      tid ← liftIO Async.myThreadId
      fail $ "Thread " <> show tid <> " has unexpectedly finished"


-- | Run a monad supplied via arguments with Matrix API HTTP request maker and authentication
withReqAndAuth
  ∷
  ( MonadIO m
  , MonadUnliftIO m
  , E.MonadThrow m
  , ML.MonadLogger m
  , MR.MonadReader r m
  , Auth.HasCredentials r
  )
  ⇒ T.EventsTimeout
  → (Api.MatrixApiClient → AuthenticatedRequest (AuthProtect "access-token") → m a)
  → m a
withReqAndAuth eventsTimeout m = do
  credentials ← MR.asks $ L.view Auth.credentials

  L.logDebug $ mconcat
    [ "Creating request handler for "
    , pack . show . T.unHomeServer . Auth.credentialsHomeServer $ credentials
    , "…"
    ]

  req ←
    let
      opts = Api.defaultRequestOptions
        { Api.requestOptionsTimeout
            = Just
            . T.Seconds
            . round @Rational @Integer
            . (* 1.5) -- Some headroom for network delays in order to avoid timeout exceptions
            . fromInteger @Rational
            . T.unSeconds
            . T.unEventsTimeout
            $ eventsTimeout
        }
    in
      Api.mkMatrixApiClient opts . Auth.credentialsHomeServer $ credentials

  auth ← Auth.getAuthenticatedMatrixRequest
  m req auth
