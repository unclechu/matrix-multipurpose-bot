{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MatrixBot.Bot.Jobs.Handlers.SendMessage
  ( sendMessage
  , MessageEdit (..)
  ) where

import qualified Control.Exception.Safe as E
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Logger as ML
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import MatrixBot.Bot.Jobs.Log (logEventResponse)
import qualified MatrixBot.Log as L
import qualified MatrixBot.MatrixApi as Api
import qualified MatrixBot.MatrixApi.Client as Api
import qualified MatrixBot.MatrixApi.Types.MEventTypes as Api
import qualified MatrixBot.SharedTypes as T
import Servant.API (AuthProtect)
import Servant.Client.Core (AuthenticatedRequest)


-- | Send a text message to a Matrix room
sendMessage
  ∷ (MonadIO m, MonadFail m, E.MonadThrow m, ML.MonadLogger m)
  ⇒ Api.MatrixApiClient
  → AuthenticatedRequest (AuthProtect "access-token")
  → T.TransactionId
  → T.RoomId
  → Maybe T.EventId
  -- ^ Reply to event
  → Maybe Text
  -- ^ Optional HTML-formatted body to be paired with plain text body
  → Text
  → Maybe MessageEdit
  → m Api.EventResponse
sendMessage req auth transactionId roomId inReplyTo htmlBody msg msgEdit = do
  L.logDebug $ mconcat
    [ "Sending message "
    , maybe mempty (("in reply to " <>) . (<> " ") . pack . show) inReplyTo
    , (pack . show) msg
    , " to room "
    , (pack . show . T.printRoomId) roomId
    , "…"
    ]

  response ←
    let proxy = Proxy @(Api.SendEventApi (Api.MEventTypeOneOf '[ 'Api.MRoomMessageType ])) in
    Api.runMatrixApiClient' req proxy $ \f → f
      auth
      roomId
      Api.MRoomMessageTypeOneOf
      transactionId
      messageContent

  response <$ logEventResponse response

  where
    messageContent =
      Api.MRoomMessageContent
        Api.MTextType
        msg
        htmlBody
        (fmap Api.InReplyTo inReplyTo)
        (fmap toEditApi msgEdit)

    toEditApi (x ∷ MessageEdit) =
      Api.MessageEdit
        Api.MTextType
        x.messageEditNewText
        x.messageEditNewHtml
        x.messageEditMessageId


data MessageEdit = MessageEdit
  { messageEditMessageId ∷ T.EventId
  , messageEditNewText ∷ Text
  , messageEditNewHtml ∷ Maybe Text
  }
  deriving stock (Show, Eq)
