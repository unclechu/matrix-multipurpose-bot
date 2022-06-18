{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module MatrixBot.Bot where

import GHC.Generics

import Data.Aeson
import Data.Function
import Data.Functor.Identity
import Data.Proxy
import Data.Text (Text, pack)
import qualified Data.List.NonEmpty as NE

import Control.Monad (void, forever, when, (<=<), forM_)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (lift)
import qualified Control.Exception.Safe as E
import qualified Control.Lens as L
import qualified Control.Monad.Logger as ML
import qualified Control.Monad.Reader as MR
import qualified Control.Monad.Trans.Except as Except

import qualified UnliftIO.Async as Async
import qualified UnliftIO.Concurrent as Async
import qualified UnliftIO.STM as STM

import qualified Network.HTTP.Types.Status as Http

import Servant.API (AuthProtect)
import Servant.Client
import Servant.Client.Core (AuthenticatedRequest)
import qualified Servant.Client.Core as Servant

import MatrixBot.AesonUtils (myGenericToJSON, myGenericParseJSON)
import MatrixBot.Log
import qualified MatrixBot.Auth as Auth
import qualified MatrixBot.MatrixApi as Api
import qualified MatrixBot.MatrixApi.Client as Api
import qualified MatrixBot.SharedTypes as T


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

startTheBot ∷ BotM r m ⇒ BotConfig → m ()
startTheBot botConfig = do
  logDebug "Starting the bot…"
  logDebug $ "Bot configuration: " <> (pack . show) botConfig
  credentials ← MR.asks $ L.view Auth.credentials

  logDebug $
    "Creating request handler for "
    <> (pack . show . T.unHomeServer . Auth.credentialsHomeServer) credentials <> "…"

  req ← Api.mkMatrixApiClient . Auth.credentialsHomeServer $ credentials
  auth ← Auth.getAuthenticatedMatrixRequest
  startupSmokeTest req auth
  jobsQueue ← STM.newTQueueIO

  logDebug "Running bot threads (jobs queue handler and room events listener)…"

  void . Async.runConcurrently
    $ Async.Concurrently (threadWrap $ jobsHandler req auth jobsQueue)
    <* Async.Concurrently (threadWrap $ eventsListener botConfig req auth jobsQueue)

  where
    startupSmokeTest req auth = do
      logDebug "Running a some test to make sure the bot is authenticated and can make Matrix API calls…"

      void . Api.runMatrixApiClient' req $
        client @Api.EventsApi Proxy auth
          Nothing
          Nothing
          (Just . T.Milliseconds $ 1)
          -- ↑ Timeouts as fast as possible (the list of received events would most likely be empty)

    logThreadFailure =
      flip E.withException $ \(e ∷ E.SomeException) →
        logError $ "The thread has failed with: " <> (pack . E.displayException) e

    threadWrap m = logThreadFailure $ m >>= \() → do
      tid ← liftIO Async.myThreadId
      fail $ "Thread " <> show tid <> " has unexpectedly finished"


-- * Bot configuration

newtype BotConfig = BotConfig
  { botConfigReactToUsers ∷ [BotConfigReactToUsers]
  }
  deriving stock (Generic, Eq, Show)

instance ToJSON BotConfig where toJSON = myGenericToJSON
instance FromJSON BotConfig where parseJSON = myGenericParseJSON


data BotConfigReactToUsers = BotConfigReactToUsers
  { botConfigReactToUsersUsersFilter ∷ Maybe (NE.NonEmpty T.Mxid)
  , botConfigReactToUsersRoomsFilter ∷ Maybe (NE.NonEmpty T.RoomId)
  , botConfigReactToUsersLeaveReactions ∷ NE.NonEmpty Text
  }
  deriving stock (Generic, Eq, Show)

instance ToJSON BotConfigReactToUsers where toJSON = myGenericToJSON
instance FromJSON BotConfigReactToUsers where parseJSON = myGenericParseJSON


-- * Bot jobs handling

type BotJobsQueue = STM.TQueue BotJob


data BotJob
  = BotJobSendReaction T.TransactionId T.RoomId T.EventId Text
  | BotJobSendMessage T.TransactionId T.RoomId Text
  deriving stock (Eq, Show)


jobsHandler
  ∷ ∀ r m
  . BotM r m
  ⇒ Api.MatrixApiClient
  → AuthenticatedRequest (AuthProtect "access-token")
  → BotJobsQueue
  → m ()
jobsHandler req auth jobsQueue = do
  logDebug "Starting bot jobs queue handler…"

  forever @m @() @() $ do
    logDebug "Waiting for next job to handle…"

    STM.atomically (STM.readTQueue jobsQueue)
      >>= (\x → x <$ logDebug ("Received a job to handle: " <> (pack . show) x))
      >>= retryOnClientError . \case
            BotJobSendReaction transactionId roomId eventId reactionText →
              sendReaction transactionId roomId eventId reactionText
            BotJobSendMessage transactionId roomId msg →
              sendMessage transactionId roomId msg

  where
    logResponse ∷ Api.EventResponse → m ()
    logResponse = logDebug . pack . show . Api.eventResponseEventId

    sendReaction ∷ T.TransactionId → T.RoomId → T.EventId → Text → m ()
    sendReaction transactionId roomId eventId reactionText = do
      logDebug $ mconcat
        [ "Sending reaction ", pack . show $ reactionText
        , " to room ", T.printRoomId roomId
        , " for ", T.unEventId eventId
        , "…"
        ]

      logResponse <=< Api.runMatrixApiClient' req $
        client @(Api.SendEventApi Api.MReactionType) Proxy auth
          roomId
          Api.MReactionType
          transactionId
          (Api.MReactionContent $ Api.RelatesTo eventId reactionText Api.MAnnotationType)

    sendMessage ∷ T.TransactionId → T.RoomId → Text → m ()
    sendMessage transactionId roomId msg = do
      logDebug $ mconcat
        [ "Sending message ", pack . show $ msg
        , " to room ", T.printRoomId roomId
        , "…"
        ]

      logResponse <=< Api.runMatrixApiClient' req $
        client @(Api.SendEventApi Api.MRoomMessageType) Proxy auth
          roomId
          Api.MRoomMessageType
          transactionId
          (Api.MRoomMessageContent Api.MTextType msg)


-- * Bot events listening

eventsListener
  ∷ ∀ r m
  . BotM r m
  ⇒ BotConfig
  → Api.MatrixApiClient
  → AuthenticatedRequest (AuthProtect "access-token")
  → BotJobsQueue
  → m ()
eventsListener botConfig req auth jobsQueue = do
  logDebug "Starting Matrix rooms events listener…"

  logDebug "Listening for events from all rooms filtering them accordingly to the bot config…"

  ($ Nothing) . fix $ \again eventToken → do
    logDebug "Waiting for next room events chunk…"

    eventsReponse ← retryOnClientError $ getNextEvents eventToken
    logDebug $ "Received next events chunk: " <> (pack . show) eventsReponse

    let events = Api.eventsResponseChunk eventsReponse
    logDebug $ "Handling " <> (pack . show . length) events <> " events…"
    mapM_ handleEvent events

    again . Just . Api.eventsResponseEnd $ eventsReponse

  where
    -- WARNING! Watch also the "Network.HTTP.Client.ResponseTimeout"! It’s 30 seconds by default.
    --          It will fail with an exception if this Matrix timeout is higher than that and after
    --          some retries fail the application completely.
    timeout = T.Seconds 20

    getNextEvents ∷ Maybe T.EventToken → m Api.EventsResponse
    getNextEvents eventToken = do
      Api.runMatrixApiClient' req $
        client @Api.EventsApi Proxy auth
          eventToken
          Nothing -- Listening for events from all rooms
          (Just . T.secondsToMilliseconds $ timeout)

    sendJob ∷ BotJob → m ()
    sendJob = STM.atomically . STM.writeTQueue jobsQueue

    handleEvent ∷ Api.ClientEvent → m ()
    handleEvent ev = do
      logDebug $ "Handling event: " <> (pack . show) ev

      case ev of
        Api.ClientEventMRoomMessage g m → do
          let
            roomId = Api.clientEventGenericRoomId g
            user = runIdentity . Api.clientEventGenericSender $ g
            eventId = runIdentity . Api.clientEventGenericEventId $ g

          logDebug $ mconcat
            [ "Received " , Api.printMRoomMessageType . Api.mRoomMessageClientEventType $ m
            , " event type from room ", T.printRoomId roomId
            , " from user ", T.printMxid user
            , " (event ID: ", T.unEventId eventId, ")"
            , " going through “react to users” configuration to see if this event is matching…"
            ]

          let
            resolveFilter =
              flip either pure $
                logDebug . ("Event mismatched filter (skipping this reaction config): " <>)

            handleReactToUsersItem reaction = (resolveFilter =<<) $ Except.runExceptT $ do
              logDebug $ "Handling reaction config: " <> (pack . show) reaction

              case botConfigReactToUsersUsersFilter reaction of
                Nothing →
                  logDebug "There is no user filter, filter passed…"
                Just x | user `elem` x →
                  logDebug $ mconcat
                    [ "User ", T.printMxid user
                    , " is one of these (filter passed): ", pack . show $ x
                    ]
                Just x →
                  Except.throwE $ mconcat
                    [ "User ", T.printMxid user
                    , " is not one of these: ", pack . show $ x
                    ]

              case botConfigReactToUsersRoomsFilter reaction of
                Nothing →
                  logDebug "There is no room filter, filter passed…"
                Just x | roomId `elem` x →
                  logDebug $ mconcat
                    [ "Room ", T.printRoomId roomId
                    , " is one of these (filter passed): ", pack . show $ x
                    ]
                Just x →
                  Except.throwE $ mconcat
                    [ "Room ", T.printRoomId roomId
                    , " is not one of these: ", pack . show $ x
                    ]

              let reactions = botConfigReactToUsersLeaveReactions reaction

              logDebug $ mconcat
                [ "Leaving ", (pack . show . NE.toList) reactions
                , " reactions for event ", T.unEventId eventId
                , " in room ", T.printRoomId roomId
                , "…"
                ]

              lift . forM_ reactions $ \reactionText → do
                transactionId ← T.genTransactionId
                sendJob $ BotJobSendReaction transactionId roomId eventId reactionText

          mapM_ handleReactToUsersItem $ botConfigReactToUsers botConfig

        Api.ClientEventOther g _ →
          logDebug $ "No need to handle " <> Api.clientEventGenericType g <> " event type (skipped)"


-- * Helpers

-- | Catch "Servant.ClientError" exception (which is probably some connectivity issue) and retry
--
-- It fails immediately if "Servant.ClientError" is an authorization failure.
-- Also fails immediately if it’s any other exception but "Servant.ClientError".
retryOnClientError
  ∷ (E.MonadMask m, ML.MonadLogger m, MonadIO m, MR.MonadReader r m, T.HasRetryParams r)
  ⇒ m a
  → m a
retryOnClientError m = do
  retriesLimit ← MR.asks $ T.unRetryLimit . L.view T.retryLimit
  retryDelay ← MR.asks $ L.view T.retryDelay
  let delay = Async.threadDelay . fromInteger . T.unMicroseconds . T.unRetryDelay $ retryDelay

  ($ retriesLimit) . fix $ \retry (pred → retryN) →
    m `E.catch` \(e ∷ Servant.ClientError) → do
      logError $ "Caught exception: " <> (pack . E.displayException) e

      case e of
        Servant.FailureResponse _req (Servant.responseStatusCode → Http.statusCode → 401) → do
          logError $
            "It seems that Matrix authorization for the bot is no longer valid, "
            <> "rethrowing exception immediately without any retries…"

          E.throwM e

        _ → pure ()

      when (retryN < 0) $ do
        logError "Retry attempts limit is hit, rethrowing exception…"
        E.throwM e

      logDebug $ mconcat
        [ "Retry attempt: "
        , pack . show $ retriesLimit - retryN
        , " of maximum "
        , pack . show $ retriesLimit
        ]

      logDebug $ "Retrying after " <> T.printRetryDelaySeconds retryDelay <> "…"
      delay

      logDebug "Waiting is done, retrying now…"
      retry retryN
