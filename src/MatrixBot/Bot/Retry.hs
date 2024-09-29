{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bot operation retrying mechanics
module MatrixBot.Bot.Retry
  ( retryOnClientError
  ) where

import Data.Function (fix)
import Data.Text (pack)
import Numeric.Natural (Natural)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Exception.Safe as E
import qualified Control.Lens as L
import qualified Control.Monad.Logger as ML
import qualified Control.Monad.Reader as MR
import qualified UnliftIO.Concurrent as Async
import qualified Network.HTTP.Types.Status as Http
import qualified Servant.Client.Core as Servant
import qualified MatrixBot.Log as L
import qualified MatrixBot.SharedTypes as T


-- | Catch "Servant.ClientError" exception (which is probably some connectivity issue) and retry
--
-- It fails immediately if "Servant.ClientError" is an authorization failure.
-- Also fails immediately if it’s any other exception but "Servant.ClientError".
retryOnClientError
  ∷ (E.MonadMask m, ML.MonadLogger m, MonadIO m, MR.MonadReader r m, T.HasRetryParams r)
  ⇒ m a
  → m a
retryOnClientError m = do
  retriesLimit ← MR.asks $ fromIntegral @Natural @Integer . T.unRetryLimit . L.view T.retryLimit
  retryDelay ← MR.asks $ L.view T.retryDelay
  let delay = Async.threadDelay . fromInteger . T.unMicroseconds . T.unRetryDelay $ retryDelay

  ($ retriesLimit) . fix $ \retry (pred → retryN) →
    m `E.catch` \(e ∷ Servant.ClientError) → do
      L.logError $ "Caught exception: " <> (pack . E.displayException) e

      case e of
        Servant.FailureResponse _req (Servant.responseStatusCode → Http.statusCode → 401) → do
          L.logError $
            "It seems that Matrix authorization for the bot is no longer valid, "
            <> "rethrowing exception immediately without any retries…"

          E.throwM e

        _ → pure ()

      when (retryN < 0) $ do
        L.logError "Retry attempts limit is hit, rethrowing exception…"
        E.throwM e

      L.logDebug $ mconcat
        [ "Retry attempt: "
        , pack . show $ retriesLimit - retryN
        , " of maximum "
        , pack . show $ retriesLimit
        ]

      L.logDebug $ "Retrying after " <> T.printRetryDelaySeconds retryDelay <> "…"
      delay

      L.logDebug "Waiting is done, retrying now…"
      retry retryN
