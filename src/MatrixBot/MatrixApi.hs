{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module MatrixBot.MatrixApi where

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types
import Data.Functor.Identity
import Data.Kind
import Data.List (find)
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Typeable
import qualified Data.Aeson.KeyMap as KM

import Servant.API

import MatrixBot.AesonUtils (myGenericToJSON, myGenericParseJSON)
import MatrixBot.MatrixApi.Types.MEventTypes
import MatrixBot.SharedTypes


-- * Basic stuff

type ClientV3 a = "_matrix" :> "client" :> "v3" :> a

type Authenticated a = AuthProtect "access-token" :> a


-- * Endpoints

-- ** Login

type LoginApi
  = ClientV3
  ( "login"
  :> ReqBody '[JSON] LoginRequest
  :> Post '[JSON] LoginResponse
  )


data LoginRequest = LoginRequest
  { loginRequestType ∷ MEventTypeOneOf '[ 'MLoginPasswordType ]
  , loginRequestUser ∷ Username
  , loginRequestPassword ∷ Password
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON LoginRequest where toJSON = myGenericToJSON
instance FromJSON LoginRequest where parseJSON = myGenericParseJSON


data LoginResponse = LoginResponse
  { loginResponseAccessToken ∷ AccessToken
  , loginResponseHomeServer ∷ HomeServer
  , loginResponseUserId ∷ Mxid
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON LoginResponse where toJSON = myGenericToJSON
instance FromJSON LoginResponse where parseJSON = myGenericParseJSON


-- * List room

-- | See the visibility of the room
type ListRoomApi
  = ClientV3
  ( Authenticated
  ( "directory"
  :> "list"
  :> "room"
  :> Capture "roomId" RoomId
  :> Get '[JSON] ListRoomResponse
  ))


newtype ListRoomResponse = ListRoomResponse
  { listRoomResponseVisibility ∷ RoomVisibility
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON ListRoomResponse where toJSON = myGenericToJSON
instance FromJSON ListRoomResponse where parseJSON = myGenericParseJSON


data RoomVisibility = RoomVisibilityPrivate | RoomVisibilityPublic
  deriving stock (Show, Eq, Enum, Bounded)

instance ToJSON RoomVisibility where
  toJSON RoomVisibilityPublic = String "public"
  toJSON RoomVisibilityPrivate = String "private"

instance FromJSON RoomVisibility where
  parseJSON ∷ ∀a. a ~ RoomVisibility ⇒ Value → Parser a
  parseJSON jsonInput
    = maybe (typeMismatch (show . typeRep $ Proxy @a) jsonInput) pure
    $ find ((jsonInput ==) . toJSON) [minBound .. maxBound ∷ a]


-- * Events

-- TODO this endpoint is deprecated, use “sync” instead
type EventsApi
  = ClientV3
  ( Authenticated
  ( "events"
  :> QueryParam "from" EventToken
  :> QueryParam "room_id" RoomId
  :> QueryParam "timeout" Milliseconds
  :> Get '[JSON] EventsResponse
  ))


data EventsResponse = EventsResponse
  { eventsResponseChunk ∷ [ClientEvent]
  , eventsResponseEnd ∷ EventToken
  , eventsResponseStart ∷ EventToken
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON EventsResponse where toJSON = myGenericToJSON
instance FromJSON EventsResponse where parseJSON = myGenericParseJSON


-- | Room event of any type
data ClientEvent
  = ClientEventMRoomMessage (ClientEventGeneric Identity) MRoomMessageClientEvent
  | ClientEventOther (ClientEventGeneric Maybe) Object
  deriving stock (Generic, Show, Eq)

instance ToJSON ClientEvent where
  toJSON (ClientEventMRoomMessage g _) = toJSON g
  toJSON (ClientEventOther _ obj) = Object obj

instance FromJSON ClientEvent where
  parseJSON ∷ ∀a. a ~ ClientEvent ⇒ Value → Parser a
  parseJSON jsonInput = case jsonInput of
    Object (KM.lookup "type" → fmap (== toJSON MRoomMessageType) → Just True) →
      ClientEventMRoomMessage
        <$> parseJSON jsonInput
        <*> parseJSON jsonInput

    Object obj → ClientEventOther <$> parseJSON jsonInput <*> pure obj
    _ → typeMismatch (show . typeRep $ Proxy @a) jsonInput


-- In the spec https://spec.matrix.org/v1.3/client-server-api/#get_matrixclientv3events
-- it says “event_id” and other fields are required but in reality some event types may not have
-- those fields or they are wrapped into “content”.
data ClientEventGeneric f = ClientEventGeneric
  { clientEventGenericType ∷ Text
  , clientEventGenericContent ∷ Object
  , clientEventGenericEventId ∷ f EventId
  , clientEventGenericOriginServerTs ∷ f Integer
  , clientEventGenericRoomId ∷ f RoomId
  , clientEventGenericSender ∷ f Mxid
  , clientEventGenericStateKey ∷ Maybe Text
  , clientEventGenericUnsigned ∷ Maybe Value
  }
  deriving stock (Generic)

deriving instance Show (ClientEventGeneric Identity)
deriving instance Show (ClientEventGeneric Maybe)

deriving instance Eq (ClientEventGeneric Identity)
deriving instance Eq (ClientEventGeneric Maybe)

instance ToJSON (ClientEventGeneric Identity) where toJSON = myGenericToJSON
instance ToJSON (ClientEventGeneric Maybe) where toJSON = myGenericToJSON

instance FromJSON (ClientEventGeneric Identity) where parseJSON = myGenericParseJSON
instance FromJSON (ClientEventGeneric Maybe) where parseJSON = myGenericParseJSON


-- | r.room.message event
data MRoomMessageClientEvent = MRoomMessageClientEvent
  { mRoomMessageClientEventType ∷ MEventTypeOneOf '[ 'MRoomMessageType ]
  , mRoomMessageClientEventContent ∷ MRoomMessageClientEventContent
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON MRoomMessageClientEvent where toJSON = myGenericToJSON
instance FromJSON MRoomMessageClientEvent where parseJSON = myGenericParseJSON


-- | r.room.message “content” field of any msgtype
data MRoomMessageClientEventContent
  = MRoomMessageClientEventContentMText MRoomMessageMTextMsgtypeClientEventContent
  | MRoomMessageClientEventContentMImage MRoomMessageMImageMsgtypeClientEventContent
  | MRoomMessageClientEventContentMVideo MRoomMessageMVideoMsgtypeClientEventContent
  | MRoomMessageClientEventContentMAudio MRoomMessageMAudioMsgtypeClientEventContent
  | MRoomMessageClientEventContentMFile MRoomMessageMFileMsgtypeClientEventContent
  | MRoomMessageClientEventContentOther Object
  deriving stock (Generic, Show, Eq)

instance ToJSON MRoomMessageClientEventContent where
  toJSON = \case
    MRoomMessageClientEventContentMText x → toJSON x
    MRoomMessageClientEventContentMImage x → toJSON x
    MRoomMessageClientEventContentMVideo x → toJSON x
    MRoomMessageClientEventContentMAudio x → toJSON x
    MRoomMessageClientEventContentMFile x → toJSON x
    MRoomMessageClientEventContentOther x → toJSON x

instance FromJSON MRoomMessageClientEventContent where
  parseJSON ∷ ∀a. a ~ MRoomMessageClientEventContent ⇒ Value → Parser a
  parseJSON jsonInput = case jsonInput of
    Object (KM.lookup "msgtype" → fmap (== toJSON MTextType) → Just True) →
      MRoomMessageClientEventContentMText <$> parseJSON jsonInput

    Object (KM.lookup "msgtype" → fmap (== toJSON MImageType) → Just True) →
      MRoomMessageClientEventContentMImage <$> parseJSON jsonInput

    Object (KM.lookup "msgtype" → fmap (== toJSON MVideoType) → Just True) →
      MRoomMessageClientEventContentMVideo <$> parseJSON jsonInput

    Object (KM.lookup "msgtype" → fmap (== toJSON MAudioType) → Just True) →
      MRoomMessageClientEventContentMAudio <$> parseJSON jsonInput

    Object (KM.lookup "msgtype" → fmap (== toJSON MFileType) → Just True) →
      MRoomMessageClientEventContentMFile <$> parseJSON jsonInput

    Object _ → MRoomMessageClientEventContentOther <$> parseJSON jsonInput

    _ → typeMismatch (show . typeRep $ Proxy @a) jsonInput


-- | m.room.message m.text msgtype content
data MRoomMessageMTextMsgtypeClientEventContent = MRoomMessageMTextMsgtypeClientEventContent
  { mRoomMessageMTextMsgtypeClientEventContentMsgtype ∷ MTextType
  , mRoomMessageMTextMsgtypeClientEventContentBody ∷ Text
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON MRoomMessageMTextMsgtypeClientEventContent where toJSON = myGenericToJSON
instance FromJSON MRoomMessageMTextMsgtypeClientEventContent where parseJSON = myGenericParseJSON


-- | m.room.message m.image msgtype content
data MRoomMessageMImageMsgtypeClientEventContent = MRoomMessageMImageMsgtypeClientEventContent
  { mRoomMessageMImageMsgtypeClientEventContentMsgtype ∷ MImageType
  , mRoomMessageMImageMsgtypeClientEventContentBody ∷ Text -- ^ File name
  , mRoomMessageMImageMsgtypeClientEventContentUrl ∷ Text -- ^ Matrix internal URL to an media file
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON MRoomMessageMImageMsgtypeClientEventContent where toJSON = myGenericToJSON
instance FromJSON MRoomMessageMImageMsgtypeClientEventContent where parseJSON = myGenericParseJSON


-- | m.room.message m.video msgtype content
data MRoomMessageMVideoMsgtypeClientEventContent = MRoomMessageMVideoMsgtypeClientEventContent
  { mRoomMessageMVideoMsgtypeClientEventContentMsgtype ∷ MVideoType
  , mRoomMessageMVideoMsgtypeClientEventContentBody ∷ Text -- ^ File name
  , mRoomMessageMVideoMsgtypeClientEventContentUrl ∷ Text -- ^ Matrix internal URL to an media file
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON MRoomMessageMVideoMsgtypeClientEventContent where toJSON = myGenericToJSON
instance FromJSON MRoomMessageMVideoMsgtypeClientEventContent where parseJSON = myGenericParseJSON


-- | m.room.message m.audio msgtype content
data MRoomMessageMAudioMsgtypeClientEventContent = MRoomMessageMAudioMsgtypeClientEventContent
  { mRoomMessageMAudioMsgtypeClientEventContentMsgtype ∷ MAudioType
  , mRoomMessageMAudioMsgtypeClientEventContentBody ∷ Text -- ^ File name
  , mRoomMessageMAudioMsgtypeClientEventContentUrl ∷ Text -- ^ Matrix internal URL to an media file
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON MRoomMessageMAudioMsgtypeClientEventContent where toJSON = myGenericToJSON
instance FromJSON MRoomMessageMAudioMsgtypeClientEventContent where parseJSON = myGenericParseJSON


-- | m.room.message m.file msgtype content
data MRoomMessageMFileMsgtypeClientEventContent = MRoomMessageMFileMsgtypeClientEventContent
  { mRoomMessageMFileMsgtypeClientEventContentMsgtype ∷ MFileType
  , mRoomMessageMFileMsgtypeClientEventContentBody ∷ Text -- ^ File name
  , mRoomMessageMFileMsgtypeClientEventContentUrl ∷ Text -- ^ Matrix internal URL to an media file
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON MRoomMessageMFileMsgtypeClientEventContent where toJSON = myGenericToJSON
instance FromJSON MRoomMessageMFileMsgtypeClientEventContent where parseJSON = myGenericParseJSON


-- * Send event

type SendEventApi t
  = ClientV3
  ( Authenticated
  ( "rooms"
  :> Capture "roomId" RoomId
  :> "send"
  :> Capture "eventType" t
  :> Capture "txnId" TransactionId
  :> ReqBody '[JSON] (EventContent t)
  :> Put '[JSON] EventResponse
  ))


type family EventContent eventType ∷ Type


newtype EventResponse = EventResponse
  { eventResponseEventId ∷ EventId
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON EventResponse where toJSON = myGenericToJSON
instance FromJSON EventResponse where parseJSON = myGenericParseJSON


-- * User filter endpoints

-- ** Create user filter

type CreateUserFilter
  = ClientV3
  ( Authenticated
  ( "user"
  :> Capture "userId" Mxid
  -- ↑ The ID of the user uploading the filter (just self MXID associated with the access token)
  :> "filter"
  :> ReqBody '[JSON] UserFilter
  :> Post '[JSON] UserFilterIdResponse
  ))


newtype UserFilterIdResponse = UserFilterIdResponse
  { userFilterIdResponse ∷ FilterId
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON UserFilterIdResponse where toJSON = myGenericToJSON
instance FromJSON UserFilterIdResponse where parseJSON = myGenericParseJSON


-- ** Get user filter

type GetUserFilter
  = ClientV3
  ( Authenticated
  ( "user"
  :> Capture "userId" Mxid
  -- ↑ The ID of the user uploading the filter (just self MXID associated with the access token)
  :> "filter"
  :> Capture "filterId" FilterId
  :> Post '[JSON] UserFilter
  ))


-- ** Shared types

-- TODO model it further
data UserFilter = UserFilter
  { userFilterAccountData ∷ Maybe () -- EventFilter
  , userFilterEventFields ∷ Maybe () -- [string]
  , userFilterEventFormat ∷ Maybe () -- enum One of: [client federation]
  , userFilterPresence ∷ Maybe () -- EventFilter
  , userFilterRoom ∷ Maybe () -- RoomFilter
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON UserFilter where toJSON = myGenericToJSON
instance FromJSON UserFilter where parseJSON = myGenericParseJSON


-- | Generic interface for @msgtype@ Matrix API string representation.
class MsgtypeString a where
  msgtypeString ∷ a → Text


-- ** m.text

data MTextType = MTextType
  deriving stock (Show, Eq, Typeable, Enum, Bounded)

instance MsgtypeString MTextType where msgtypeString MTextType = "m.text"
instance ToHttpApiData MTextType where toUrlPiece = msgtypeString
instance ToJSON MTextType where toJSON = String . msgtypeString
instance FromJSON MTextType where parseJSON = mTypeGenericParseJSON


-- ** m.image

data MImageType = MImageType
  deriving stock (Show, Eq, Typeable, Enum, Bounded)

instance MsgtypeString MImageType where msgtypeString MImageType = "m.image"
instance ToHttpApiData MImageType where toUrlPiece = msgtypeString
instance ToJSON MImageType where toJSON = String . msgtypeString
instance FromJSON MImageType where parseJSON = mTypeGenericParseJSON


-- ** m.video

data MVideoType = MVideoType
  deriving stock (Show, Eq, Typeable, Enum, Bounded)

instance MsgtypeString MVideoType where msgtypeString MVideoType = "m.video"
instance ToHttpApiData MVideoType where toUrlPiece = msgtypeString
instance ToJSON MVideoType where toJSON = String . msgtypeString
instance FromJSON MVideoType where parseJSON = mTypeGenericParseJSON


-- ** m.audio

data MAudioType = MAudioType
  deriving stock (Show, Eq, Typeable, Enum, Bounded)

instance MsgtypeString MAudioType where msgtypeString MAudioType = "m.audio"
instance ToHttpApiData MAudioType where toUrlPiece = msgtypeString
instance ToJSON MAudioType where toJSON = String . msgtypeString
instance FromJSON MAudioType where parseJSON = mTypeGenericParseJSON


-- ** m.file

data MFileType = MFileType
  deriving stock (Show, Eq, Typeable, Enum, Bounded)

instance MsgtypeString MFileType where msgtypeString MFileType = "m.file"
instance ToHttpApiData MFileType where toUrlPiece = msgtypeString
instance ToJSON MFileType where toJSON = String . msgtypeString
instance FromJSON MFileType where parseJSON = mTypeGenericParseJSON


-- ** m.room.message content

data MRoomMessageContent = MRoomMessageContent
  { mRoomMessageContentMsgtype ∷ MTextType
  , mRoomMessageContentBody ∷ Text
  , mRoomMessageContentMRelatesTo ∷ Maybe InReplyTo
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON MRoomMessageContent where
  toJSON x = object $
    [ "msgtype" .= mRoomMessageContentMsgtype x
    , "body" .= mRoomMessageContentBody x
    ]
    <>
    case mRoomMessageContentMRelatesTo x of
      Nothing → mempty
      Just y →
        [ mRelatedToKey .= object
            [ mInReplyToKey .= toJSON y
            ]
        ]

instance FromJSON MRoomMessageContent where
  parseJSON jsonValue@(Object x) = MRoomMessageContent
    <$> x .: "msgtype"
    <*> x .: "body"
    <*> inReplyToParser
    where
      inReplyToParser =
        x .:? mRelatedToKey >>= \case
          Nothing → pure Nothing
          Just Null → pure Nothing
          Just (Object y) → y .: mInReplyToKey >>= fmap Just . parseJSON @InReplyTo
          _ → fail $ "Failed to parse MRoomMessageContent from " <> show jsonValue

  parseJSON jsonValue = fail $ "Failed to parse MRoomMessageContent from " <> show jsonValue


type instance EventContent (MEventTypeOneOf '[ 'MRoomMessageType ]) = MRoomMessageContent


newtype InReplyTo = InReplyTo
  { inReplyToEventId ∷ EventId
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON InReplyTo where toJSON = myGenericToJSON
instance FromJSON InReplyTo where parseJSON = myGenericParseJSON


mInReplyToKey ∷ IsString s ⇒ s
mInReplyToKey = "m.in_reply_to"


--- *** Content

newtype MReactionContent = MReactionContent
  { mReactionContentMRelatesTo ∷ RelatesTo
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON MReactionContent where
  toJSON x = object [mRelatedToKey .= mReactionContentMRelatesTo x]

instance FromJSON MReactionContent where
  parseJSON ∷ ∀a. (a ~ MReactionContent) ⇒ Value → Parser a
  parseJSON = withObject (show . typeRep $ Proxy @a) $ \v → MReactionContent <$> v .: mRelatedToKey

mRelatedToKey ∷ IsString s ⇒ s
mRelatedToKey = "m.relates_to"


type instance EventContent (MEventTypeOneOf '[ 'MReactionType ]) = MReactionContent


data RelatesTo = RelatesTo
  { relatesToEventId ∷ EventId
  , relatesToKey ∷ Text
  , relatesToRelType ∷ MEventTypeOneOf '[ 'MAnnotationType ]
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON RelatesTo where toJSON = myGenericToJSON
instance FromJSON RelatesTo where parseJSON = myGenericParseJSON


-- * Helpers

mTypeGenericParseJSON ∷ ∀a. (Bounded a, Enum a, Typeable a, ToJSON a) ⇒ Value → Parser a
mTypeGenericParseJSON jsonInput
  = maybe (typeMismatch (show . typeRep $ Proxy @a) jsonInput) pure
  $ find ((jsonInput ==) . toJSON) [minBound .. maxBound ∷ a]
