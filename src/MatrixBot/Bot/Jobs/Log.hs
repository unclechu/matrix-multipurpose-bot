{-# LANGUAGE UnicodeSyntax #-}

-- | Logging-related helpers for the Bot Jobs implementation
module MatrixBot.Bot.Jobs.Log
  ( logEventResponse
  ) where

import Data.Text (pack)
import qualified Control.Monad.Logger as ML
import qualified MatrixBot.MatrixApi as Api
import qualified MatrixBot.Log as L


-- | Log Bot Job being handled event (job event response ID)
logEventResponse ∷ ML.MonadLogger m ⇒ Api.EventResponse → m ()
logEventResponse = L.logDebug . pack . show . Api.eventResponseEventId
