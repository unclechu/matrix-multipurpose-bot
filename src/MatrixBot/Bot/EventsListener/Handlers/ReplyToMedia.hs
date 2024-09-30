{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module MatrixBot.Bot.EventsListener.Handlers.ReplyToMedia
  ( replyToMedia
  ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Logger as ML
import qualified Control.Monad.Trans.Except as Except
import qualified Data.Attoparsec.Text as AP
import Data.Text (pack, Text)
import qualified MatrixBot.Bot.BotConfig as BotConfig
import MatrixBot.Bot.BotM (BotM)
import qualified MatrixBot.Bot.EventsListener.Filters as Filters
import MatrixBot.Bot.Jobs.BotJob (BotJob (BotJobSendMessage))
import MatrixBot.Bot.Jobs.Queue (HasBotJobsWriter (botJobsWriter))
import qualified MatrixBot.Log as L
import qualified MatrixBot.MatrixApi as Api
import qualified MatrixBot.SharedTypes as T
import qualified UnliftIO.STM as STM


-- | Process @m.room.message@ event and see if there is a need to reply to an event with a message
--   template.
replyToMedia
  ∷ ∀r m. (BotM r m, HasBotJobsWriter r)
  ⇒ [BotConfig.BotConfigReplyToMedia]
  -- ^ Bot configuration for “react to users” feature
  → T.RoomId
  -- ^ Matrix room where there event was sent to
  → T.Mxid
  -- ^ User ID (who sent the event)
  → T.EventId
  -- ^ Unique identifier of the event
  → Api.MRoomMessageClientEventContent
  -- ^ Client content for the event
  → m ()
replyToMedia [] _ _ _ _ =
  L.logDebug $ mconcat
    [ "There is no configuration entries for “reply to media” feature. "
    , "No replies/messages will be posted for the event."
    ]
replyToMedia configEntries roomId userId eventId clientContent = do
  case clientContent of
    (mkMediaEventDataFromClientContent → Just mediaData) →
      withExtractedMediaValues mediaData $ \extractMediaValues →
        logProcessing >> mapM_ (handleEntry mediaData extractMediaValues) configEntries
    _ →
      L.logDebug $ mconcat
        [ "This event type is not a detected media file one. "
        , "“Reply to media” handling is skipped for this event."
        ]

  where
    logProcessing =
      L.logDebug "Going through “reply to media” configuration to see if this event is matching…"

    resolveFilter =
      flip either pure $
        L.logDebug . ("Event mismatched filter (skipping this replying to media config entry): " <>)

    handleEntry ∷ MediaEventData → ExtractedMediaValues → BotConfig.BotConfigReplyToMedia → m ()
    handleEntry mediaEventData extractedMediaValues entry = (resolveFilter =<<) $ Except.runExceptT $ do
      L.logDebug $ "Handling reply to media config entry: " <> (pack . show) entry

      Filters.filterByUser (BotConfig.botConfigReplyToMediaUsersFilter entry) userId
      Filters.filterByRoom (BotConfig.botConfigReplyToMediaRoomsFilter entry) roomId

      L.logWarn $ pack . show $ mediaEventData
      L.logWarn $ pack . show $ extractedMediaValues

      let
        replyMessage =
          renderTemplate
            (BotConfig.botConfigReplyToMediaMessageTemplate entry)
            mediaEventData
            extractedMediaValues

      L.logDebug $ mconcat
        [ "Sending a reply message ", (pack . show) replyMessage
        , " for event ", T.unEventId eventId
        , " in room ", T.printRoomId roomId
        , "…"
        ]

      transactionId ← T.genTransactionId
      Lens.view botJobsWriter >>= \sendJob →
        STM.atomically . sendJob $
          -- TODO: This is just a message, make it be a reply
          BotJobSendMessage transactionId roomId replyMessage


renderTemplate
  ∷ [BotConfig.BotConfigReplyToMedia_MessageTemplateEntry]
  → MediaEventData
  → ExtractedMediaValues
  → Text
renderTemplate template mediaEventData extractedMediaValues =
  flip foldMap template $ \case
    BotConfig.BotConfigReplyToMedia_MessageTemplateEntry_PlainString x → x
    BotConfig.BotConfigReplyToMedia_MessageTemplateEntry_DynamicSubstitution_Field x →
      case x of
        BotConfig.BotConfigReplyToMedia_DynamicFieldName_MsgType → msgtype mediaEventData
        BotConfig.BotConfigReplyToMedia_DynamicFieldName_Body → body mediaEventData
        BotConfig.BotConfigReplyToMedia_DynamicFieldName_Url → url mediaEventData
    BotConfig.BotConfigReplyToMedia_MessageTemplateEntry_DynamicSubstitution_ExtractedValue x →
      case x of
        BotConfig.BotConfigReplyToMedia_DynamicExtractedValueName_MediaId →
          extractedMediaId extractedMediaValues


data MediaEventData = MediaEventData
  { msgtype ∷ Text
  , body ∷ Text -- ^ File name
  , url ∷ Text
  }
  deriving stock (Show, Eq)


mkMediaEventDataFromClientContent ∷ Api.MRoomMessageClientEventContent → Maybe MediaEventData
mkMediaEventDataFromClientContent = \case
  Api.MRoomMessageClientEventContentMImage x →
    Just $ MediaEventData
      { msgtype = (Api.msgtypeString . Api.mRoomMessageMImageMsgtypeClientEventContentMsgtype) x
      , body = Api.mRoomMessageMImageMsgtypeClientEventContentBody x
      , url = Api.mRoomMessageMImageMsgtypeClientEventContentUrl x
      }
  Api.MRoomMessageClientEventContentMVideo x →
    Just $ MediaEventData
      { msgtype = (Api.msgtypeString . Api.mRoomMessageMVideoMsgtypeClientEventContentMsgtype) x
      , body = Api.mRoomMessageMVideoMsgtypeClientEventContentBody x
      , url = Api.mRoomMessageMVideoMsgtypeClientEventContentUrl x
      }
  Api.MRoomMessageClientEventContentMAudio x →
    Just $ MediaEventData
      { msgtype = (Api.msgtypeString . Api.mRoomMessageMAudioMsgtypeClientEventContentMsgtype) x
      , body = Api.mRoomMessageMAudioMsgtypeClientEventContentBody x
      , url = Api.mRoomMessageMAudioMsgtypeClientEventContentUrl x
      }
  _ → Nothing


-- | Some details extracted out of "MediaEventData" value
newtype ExtractedMediaValues = ExtractedMediaValues
  { extractedMediaId ∷ Text
  }
  deriving stock (Show, Eq)


withExtractedMediaValues
  ∷ ML.MonadLogger m
  ⇒ MediaEventData
  → (ExtractedMediaValues → m ())
  → m ()
withExtractedMediaValues mediaEventData m = do
  case extractMediaIdFromUrl $ url mediaEventData of
    Right x →
      m ExtractedMediaValues
        { extractedMediaId = x
        }
    Left e →
      L.logError $ mconcat
        [ "Failed to extract “media_id” out from “url”: ", (pack . show) e
        , " (skipping the event for “reply to media” handling)"
        ]


extractMediaIdFromUrl ∷ Text → Either String Text
extractMediaIdFromUrl = AP.parseOnly parser
  where
    parser ∷ AP.Parser Text
    parser
      = AP.string "mxc://"
      *> AP.many1 (AP.many1 (AP.satisfy (/= '/')) *> AP.char '/')
      *> AP.takeText
      <* AP.endOfInput
