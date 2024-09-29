{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MatrixBot.Bot.EventsListener.Handlers.ReactToUsers
  ( reactToUsers
  ) where

import qualified Control.Lens as Lens
import Control.Monad (forM_)
import Control.Monad.Trans (lift)
import qualified Control.Monad.Trans.Except as Except
import qualified Data.List.NonEmpty as NE
import Data.Text (pack)
import MatrixBot.Bot.BotConfig (BotConfigReactToUsers (..))
import MatrixBot.Bot.BotM (BotM)
import MatrixBot.Bot.Jobs.BotJob (BotJob (BotJobSendReaction))
import qualified MatrixBot.Log as L
import qualified MatrixBot.SharedTypes as T
import qualified UnliftIO.STM as STM
import MatrixBot.Bot.Jobs.Queue (HasBotJobsWriter (botJobsWriter))
import qualified MatrixBot.Bot.EventsListener.Filters as Filters


-- | Process @m.room.message@ event and see if there is a need to leave a reaction for it.
reactToUsers
  ∷ (BotM r m, HasBotJobsWriter r)
  ⇒ [BotConfigReactToUsers]
  -- ^ Bot configuration for “react to users” feature
  → T.RoomId
  -- ^ Matrix room where there event was sent to
  → T.Mxid
  -- ^ User ID (who sent the event)
  → T.EventId
  -- ^ Unique identifier of the event
  → m ()
reactToUsers [] _ _ _ =
  L.logDebug $ mconcat
    [ "There is no configuration entries for “react to users” feature. "
    , "No reactions will be left for the event."
    ]
reactToUsers configEntries roomId userId eventId = do
  L.logDebug "Going through “react to users” configuration to see if this event is matching…"
  mapM_ handleEntry configEntries

  where
    resolveFilter =
      flip either pure $
        L.logDebug . ("Event mismatched filter (skipping this reaction config entry): " <>)

    handleEntry entry = (resolveFilter =<<) $ Except.runExceptT $ do
      L.logDebug $ "Handling reaction config entry: " <> (pack . show) entry

      Filters.filterByUser (botConfigReactToUsersUsersFilter entry) userId
      Filters.filterByRoom (botConfigReactToUsersRoomsFilter entry) roomId

      let reactions = botConfigReactToUsersLeaveReactions entry

      L.logDebug $ mconcat
        [ "Leaving ", (pack . show . NE.toList) reactions
        , " reactions for event ", T.unEventId eventId
        , " in room ", T.printRoomId roomId
        , "…"
        ]

      lift . forM_ reactions $ \reactionText → do
        transactionId ← T.genTransactionId
        Lens.view botJobsWriter >>= \sendJob →
          STM.atomically . sendJob $
            BotJobSendReaction transactionId roomId eventId reactionText
