{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main handling entrypoint, other handlers are brached from here.
module MatrixBot.Bot.Jobs.Handlers.MainHandler
  ( jobsHandler
  ) where

import MatrixBot.Bot.BotM (BotM)
import qualified MatrixBot.MatrixApi.Client as Api
import Servant.Client.Core (AuthenticatedRequest)
import Servant.API (AuthProtect)
import MatrixBot.Bot.Jobs.Queue (HasBotJobsReader (botJobsReader))
import MatrixBot.Bot.Jobs.BotJob (BotJob (..))
import qualified MatrixBot.Log as L
import Control.Monad (forever)
import qualified UnliftIO.STM as STM
import Data.Text (pack)
import MatrixBot.Bot.Retry (retryOnClientError)
import Data.Functor (void)
import MatrixBot.Bot.Jobs.Handlers.SendReaction (sendReaction)
import MatrixBot.Bot.Jobs.Handlers.SendMessage (sendMessage)
import qualified Control.Lens as Lens


-- | Bot Jobs handler (infinite loop of reading new jobs and performing them)
jobsHandler
  ∷ ∀ r m
  . (BotM r m, HasBotJobsReader r)
  ⇒ Api.MatrixApiClient
  → AuthenticatedRequest (AuthProtect "access-token")
  → m ()
jobsHandler req auth = do
  L.logDebug "Starting bot jobs queue handler…"

  forever @m @() @() $ do
    L.logDebug "Waiting for next job to handle…"

    Lens.view botJobsReader
      >>= STM.atomically
      >>= (\x → x <$ L.logDebug ("Received a job to handle: " <> (pack . show) x))
      >>= retryOnClientError . \case
            BotJobSendReaction transactionId roomId eventId reactionText →
              void $ sendReaction req auth transactionId roomId eventId reactionText
            BotJobSendMessage transactionId roomId inReplyTo htmlBody msg →
              void $ sendMessage req auth transactionId roomId inReplyTo htmlBody msg
