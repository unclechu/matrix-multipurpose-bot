{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DerivingStrategies #-}

module MatrixBot.Bot.Jobs.Queue
  ( BotJobsQueue (..)
  , mkBotJobsQueue
  , HasBotJobsReader (..)
  , HasBotJobsWriter (..)
  ) where

import qualified UnliftIO.STM as STM
import Control.Monad.IO.Class (MonadIO)
import MatrixBot.Bot.Jobs.BotJob (BotJob)
import Data.Functor ((<&>))
import qualified Control.Lens.Getter as Lens


-- | A queue of Bot Jobs interface
data BotJobsQueue = BotJobsQueue
  { sendJob ∷ BotJob → STM.STM ()
  , getNextJob ∷ STM.STM BotJob
  -- ^ Blocking operation (will wait for another job to appear)
  }


mkBotJobsQueue ∷ MonadIO m ⇒ m BotJobsQueue
mkBotJobsQueue =
  STM.newTQueueIO <&> \jobsQueue ->
    BotJobsQueue
      { sendJob = STM.writeTQueue jobsQueue
      , getNextJob = STM.readTQueue jobsQueue
      }


class HasBotJobsReader r where
  botJobsReader ∷ Lens.Getter r (STM.STM BotJob)

instance HasBotJobsReader BotJobsQueue where
  botJobsReader = Lens.to getNextJob


class HasBotJobsWriter r where
  botJobsWriter ∷ Lens.Getter r (BotJob → STM.STM ())

instance HasBotJobsWriter BotJobsQueue where
  botJobsWriter = Lens.to sendJob
