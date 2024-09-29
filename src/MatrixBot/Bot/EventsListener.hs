{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <&>" #-}

module MatrixBot.Bot.EventsListener
  ( eventsListener
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except as Except
import Data.Aeson (eitherDecodeStrict, encodeFile)
import qualified Data.ByteString as BS
import Data.Data (Proxy(Proxy))
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity(runIdentity))
import qualified Data.List.NonEmpty as NE
import Data.Text (pack)
import qualified MatrixBot.Bot.BotConfig as BotConfig
import MatrixBot.Bot.BotM (BotM)
import MatrixBot.Bot.Jobs.BotJob (BotJob (BotJobSendReaction))
import MatrixBot.Bot.Retry (retryOnClientError)
import qualified MatrixBot.Log as L
import qualified MatrixBot.MatrixApi as Api
import qualified MatrixBot.MatrixApi.Client as Api
import qualified MatrixBot.MatrixApi.Types.MEventTypes as Api
import qualified MatrixBot.SharedTypes as T
import Servant.API (AuthProtect)
import Servant.Client.Core (AuthenticatedRequest)
import System.Directory (doesFileExist)
import qualified UnliftIO.STM as STM
import MatrixBot.Bot.Jobs.Queue (HasBotJobsWriter (botJobsWriter))
import qualified Control.Lens as Lens


-- | Matrix rooms events listener handler
--
-- Read new chunks of events and process them in an infinite loop.
eventsListener
  ∷ ∀ r m
  . (BotM r m, HasBotJobsWriter r)
  ⇒ Maybe FilePath
  → T.EventsTimeout
  → BotConfig.BotConfig
  → Api.MatrixApiClient
  → AuthenticatedRequest (AuthProtect "access-token")
  → m ()
eventsListener eventTokenFile eventsTimeout botConfig req auth = do
  L.logDebug "Starting Matrix rooms events listener…"

  -- Preserve last handled event checkpoint feature.
  --
  -- Will read last event identity/token from a file and request next chunk of
  -- events from that point. So if bot was offline due to a server reboot or
  -- other reason it will still get the events added to the room during the
  -- bot’s absence.
  --
  -- After each events chunk handle the new token is written to the file.
  --
  -- You can always skip the accumulated events by just deleting that file.
  --
  -- TODO: Implement debouncer feature. If there were a lot of events
  --       accumulated when the bot was offline then there can be a lot of
  --       traffic generated of reading chunks of events, handling them, and
  --       then again and again. So that the server may start blocking your
  --       spamming traffic, and the bot would stuck in a retry loop, and
  --       eventually critically fail when retry limit is reached.
  --       Implement a request debounching feature so that there would be some
  --       configured interval between requests to the Matrix homeserver.
  initialEventToken ←
    case eventTokenFile of
      Nothing → do
        L.logWarn "No event token file provided, listening starting from next following events…"
        pure Nothing
      Just file → do
        L.logDebug $ mconcat
          [ "Checking if ", pack . show $ eventTokenFile
          , " token file exists to read initial event token from…"
          ]

        exists ← liftIO (doesFileExist file)

        if not exists
        then
          (Nothing <$) . L.logDebug $ mconcat
            [ "Event token file ", pack . show $ eventTokenFile
            , " does not exist, listening starting from next following events…"
            ]
        else
          liftIO (BS.readFile file) <&> eitherDecodeStrict >>= \case
            Right x →
              (Just x <$) . L.logDebug $ mconcat
                [ "Read and parsed last event token from ", pack . show $ file
                , " file, listening for next events starting from: ", pack . show . T.unEventToken $ x
                ]
            Left e →
              fail $ mconcat
                [ "Failed to read event token from ", show file
                , " event token file: ", e
                ]

  L.logDebug "Listening for events from all rooms filtering them accordingly to the bot config…"

  -- Start an infinite loop of events chunks handling.
  ($ initialEventToken) . fix $ \again eventToken → do
    L.logDebug "Waiting for next room events chunk…"

    eventsReponse ← retryOnClientError $ getNextEvents' eventToken
    L.logDebug $ "Received next events chunk: " <> (pack . show) eventsReponse

    let events = Api.eventsResponseChunk eventsReponse
    L.logDebug $ "Handling " <> (pack . show . length) events <> " events…"
    mapM_ (handleEvent botConfig) events

    let nextEventToken = Api.eventsResponseEnd eventsReponse

    -- Preserve last read event token by saving in to a file.
    case eventTokenFile of
      Nothing → pure ()
      Just _ | eventToken == Just nextEventToken →
        L.logDebug $ mconcat
          [ "Received event token ", pack . show . T.unEventToken $ nextEventToken
          , "didn’t change since the last time, no need to save it again, skipping save…"
          ]
      Just _ | null events →
        L.logDebug
          "Received list of events is empty, skipping save of event token to event token file…"
      Just file → do
        L.logDebug $ mconcat
          [ "Saving received event token ", pack . show . T.unEventToken $ nextEventToken
          , " to ", pack . show $ eventTokenFile, " event token file…"
          ]
        liftIO $ encodeFile file nextEventToken

    again . Just $ nextEventToken

  where
    getNextEvents' ∷ Maybe T.EventToken → m Api.EventsResponse
    getNextEvents' = getNextEvents eventsTimeout req auth


-- | Get next chunk of Matrix room events
--
-- Blocking operation. Will wait for new events to appear.
-- But will return an empty list of events if timeout is hit.
getNextEvents
  ∷ BotM r m
  ⇒ T.EventsTimeout
  → Api.MatrixApiClient
  → AuthenticatedRequest (AuthProtect "access-token")
  → Maybe T.EventToken
  → m Api.EventsResponse
getNextEvents eventsTimeout req auth eventToken =
  Api.runMatrixApiClient' req (Proxy @Api.EventsApi) $ \f → f
    auth
    eventToken
    Nothing -- Listening for events from all rooms
    (Just . T.secondsToMilliseconds . T.unEventsTimeout $ eventsTimeout)


-- | Process one read event and react to it if event filters are matching.
handleEvent ∷ (BotM r m, HasBotJobsWriter r) ⇒ BotConfig.BotConfig → Api.ClientEvent → m ()
handleEvent botConfig ev = do
  L.logDebug $ "Handling event: " <> (pack . show) ev

  case ev of
    Api.ClientEventMRoomMessage g m → do
      let
        roomId = runIdentity . Api.clientEventGenericRoomId $ g
        user = runIdentity . Api.clientEventGenericSender $ g
        eventId = runIdentity . Api.clientEventGenericEventId $ g

      L.logDebug $ mconcat
        [ "Received "
        , Api.mEventTypeToString . Api.mEventTypeOneOfToMEventType . Api.mRoomMessageClientEventType $ m
        , " event type from room ", T.printRoomId roomId
        , " from user ", T.printMxid user
        , " (event ID: ", T.unEventId eventId, ")"
        , " going through “react to users” configuration to see if this event is matching…"
        ]

      let
        resolveFilter =
          flip either pure $
            L.logDebug . ("Event mismatched filter (skipping this reaction config): " <>)

        handleReactToUsersItem reaction = (resolveFilter =<<) $ Except.runExceptT $ do
          L.logDebug $ "Handling reaction config: " <> (pack . show) reaction

          case BotConfig.botConfigReactToUsersUsersFilter reaction of
            Nothing →
              L.logDebug "There is no user filter, filter passed…"
            Just x | user `elem` x →
              L.logDebug $ mconcat
                [ "User ", T.printMxid user
                , " is one of these (filter passed): ", pack . show $ x
                ]
            Just x →
              Except.throwE $ mconcat
                [ "User ", T.printMxid user
                , " is not one of these: ", pack . show $ x
                ]

          case BotConfig.botConfigReactToUsersRoomsFilter reaction of
            Nothing →
              L.logDebug "There is no room filter, filter passed…"
            Just x | roomId `elem` x →
              L.logDebug $ mconcat
                [ "Room ", T.printRoomId roomId
                , " is one of these (filter passed): ", pack . show $ x
                ]
            Just x →
              Except.throwE $ mconcat
                [ "Room ", T.printRoomId roomId
                , " is not one of these: ", pack . show $ x
                ]

          let reactions = BotConfig.botConfigReactToUsersLeaveReactions reaction

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

      mapM_ handleReactToUsersItem $ BotConfig.botConfigReactToUsers botConfig

    Api.ClientEventOther g _ →
      L.logDebug $ "No need to handle " <> Api.clientEventGenericType g <> " event type (skipped)"
