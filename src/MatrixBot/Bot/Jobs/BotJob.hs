{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DerivingStrategies #-}

module MatrixBot.Bot.Jobs.BotJob
  ( BotJob (..)
  ) where

import qualified MatrixBot.SharedTypes as T
import Data.Text (Text)


-- | A single Bot Job
data BotJob
  = BotJobSendReaction T.TransactionId T.RoomId T.EventId Text
  | BotJobSendMessage T.TransactionId T.RoomId (Maybe T.EventId) Text
  -- ^ Send a message (@Maybe T.EventId@ means in reply to this event ID)
  deriving stock (Eq, Show)
