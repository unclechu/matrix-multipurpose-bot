{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module MatrixBot.Bot.SmokeTest
  ( startupSmokeTest
  ) where

import Data.Proxy (Proxy (Proxy))
import Control.Monad (void)
import Servant.API (AuthProtect)
import Servant.Client.Core (AuthenticatedRequest)
import qualified MatrixBot.Log as L
import qualified MatrixBot.MatrixApi as Api
import qualified MatrixBot.MatrixApi.Client as Api
import qualified MatrixBot.SharedTypes as T
import MatrixBot.Bot.BotM (BotM)


-- | A smoke test to run to check Matrix API can be called using current authentication.
--
-- Intended to be run at startup, before doing anything else.
startupSmokeTest
  ∷ BotM r m
  ⇒ Api.MatrixApiClient
  → AuthenticatedRequest (AuthProtect "access-token")
  → m ()
startupSmokeTest req auth = do
  L.logDebug "Running a some test to make sure the bot is authenticated and can make Matrix API calls…"

  void $ Api.runMatrixApiClient' req (Proxy @Api.EventsApi) $ \f → f
    auth
    Nothing
    Nothing
    (Just . T.Milliseconds $ 1)
    -- ↑ Timeouts as fast as possible (the list of received events would most likely be empty)
