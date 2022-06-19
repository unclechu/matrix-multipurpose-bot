{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

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


data MLoginPasswordType = MLoginPasswordType
  deriving stock (Show, Eq, Typeable, Enum, Bounded)

instance ToHttpApiData MLoginPasswordType where toUrlPiece = printMLoginPasswordType
instance ToJSON MLoginPasswordType where toJSON = String . printMLoginPasswordType
instance FromJSON MLoginPasswordType where parseJSON = mTypeGenericParseJSON

printMLoginPasswordType ∷ MLoginPasswordType → Text
printMLoginPasswordType MLoginPasswordType = "m.login.password"


data LoginRequest = LoginRequest
  { loginRequestType ∷ MLoginPasswordType
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
  { mRoomMessageClientEventType ∷ MRoomMessageType
  , mRoomMessageClientEventContent ∷ MRoomMessageClientEventContent
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON MRoomMessageClientEvent where toJSON = myGenericToJSON
instance FromJSON MRoomMessageClientEvent where parseJSON = myGenericParseJSON


-- | r.room.message “content” field of any msgtype
data MRoomMessageClientEventContent
  = MRoomMessageClientEventContentMText MRoomMessageMTextMsgtypeClientEventContent
  | MRoomMessageClientEventContentOther MRoomMessageOtherMsgtypeClientEventContent
  deriving stock (Generic, Show, Eq)

instance ToJSON MRoomMessageClientEventContent where
  toJSON = \case
    MRoomMessageClientEventContentMText x → toJSON x
    MRoomMessageClientEventContentOther x → toJSON x

instance FromJSON MRoomMessageClientEventContent where
  parseJSON ∷ ∀a. a ~ MRoomMessageClientEventContent ⇒ Value → Parser a
  parseJSON jsonInput = case jsonInput of
    Object (KM.lookup "msgtype" → fmap (== toJSON MTextType) → Just True) →
      MRoomMessageClientEventContentMText <$> parseJSON jsonInput

    Object _ → MRoomMessageClientEventContentOther <$> parseJSON jsonInput
    _ → typeMismatch (show . typeRep $ Proxy @a) jsonInput


-- | m.room.message m.text msgtype
data MRoomMessageMTextMsgtypeClientEventContent = MRoomMessageMTextMsgtypeClientEventContent
  { mRoomMessageMTextMsgtypeClientEventContentMsgtype ∷ MTextType
  , mRoomMessageMTextMsgtypeClientEventContentBody ∷ Text
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON MRoomMessageMTextMsgtypeClientEventContent where toJSON = myGenericToJSON
instance FromJSON MRoomMessageMTextMsgtypeClientEventContent where parseJSON = myGenericParseJSON


-- | m.room.message any other unmatched msgtype
data MRoomMessageOtherMsgtypeClientEventContent = MRoomMessageOtherMsgtypeClientEventContent
  { mRoomMessageOtherMsgtypeClientEventContentMsgtype ∷ Text
  , mRoomMessageOtherMsgtypeClientEventContentBody ∷ Text
  , mRoomMessageOtherMsgtypeClientEventContentOther ∷ Object
  -- ^ All other fields
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON MRoomMessageOtherMsgtypeClientEventContent where
  toJSON x = Object $ mconcat
    [ KM.singleton "msgtype" . String . mRoomMessageOtherMsgtypeClientEventContentMsgtype $ x
    , KM.singleton "body" . String . mRoomMessageOtherMsgtypeClientEventContentBody $ x
    , mRoomMessageOtherMsgtypeClientEventContentOther x
    ]

instance FromJSON MRoomMessageOtherMsgtypeClientEventContent where
  parseJSON ∷ ∀a. (a ~ MRoomMessageOtherMsgtypeClientEventContent) ⇒ Value → Parser a
  parseJSON = withObject (show . typeRep $ Proxy @a) $ \v →
    MRoomMessageOtherMsgtypeClientEventContent
      <$> v .: msgtypeKey
      <*> v .: bodyKey
      <*> pure (KM.delete msgtypeKey . KM.delete bodyKey $ v)
    where
      msgtypeKey = "msgtype"
      bodyKey = "body"


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


-- ** m.text

data MTextType = MTextType
  deriving stock (Show, Eq, Typeable, Enum, Bounded)

instance ToHttpApiData MTextType where toUrlPiece = printMTextType
instance ToJSON MTextType where toJSON = String . printMTextType
instance FromJSON MTextType where parseJSON = mTypeGenericParseJSON

printMTextType ∷ MTextType → Text
printMTextType MTextType = "m.text"


-- ** m.room.message


data MRoomMessageType = MRoomMessageType
  deriving stock (Show, Eq, Typeable, Enum, Bounded)

instance ToHttpApiData MRoomMessageType where toUrlPiece = printMRoomMessageType
instance ToJSON MRoomMessageType where toJSON = String . printMRoomMessageType
instance FromJSON MRoomMessageType where parseJSON = mTypeGenericParseJSON

printMRoomMessageType ∷ MRoomMessageType → Text
printMRoomMessageType MRoomMessageType = "m.room.message"


-- ** m.room.message content

data MRoomMessageContent = MRoomMessageContent
  { mRoomMessageContentMsgtype ∷ MTextType
  , mRoomMessageContentBody ∷ Text
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON MRoomMessageContent where toJSON = myGenericToJSON
instance FromJSON MRoomMessageContent where parseJSON = myGenericParseJSON


type instance EventContent MRoomMessageType = MRoomMessageContent


-- ** m.reaction

-- *** Type

data MReactionType = MReactionType
  deriving stock (Show, Eq, Typeable, Enum, Bounded)

instance ToHttpApiData MReactionType where toUrlPiece = printMReactionType
instance ToJSON MReactionType where toJSON = String . printMReactionType
instance FromJSON MReactionType where parseJSON = mTypeGenericParseJSON

printMReactionType ∷ MReactionType → Text
printMReactionType MReactionType = "m.reaction"


data MAnnotationType = MAnnotationType
  deriving stock (Show, Eq, Typeable, Enum, Bounded)

instance ToHttpApiData MAnnotationType where toUrlPiece = printMAnnotationType
instance ToJSON MAnnotationType where toJSON = String . printMAnnotationType
instance FromJSON MAnnotationType where parseJSON = mTypeGenericParseJSON

printMAnnotationType ∷ MAnnotationType → Text
printMAnnotationType MAnnotationType = "m.annotation"


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


type instance EventContent MReactionType = MReactionContent


data RelatesTo = RelatesTo
  { relatesToEventId ∷ EventId
  , relatesToKey ∷ Text
  , relatesToRelType ∷ MAnnotationType
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON RelatesTo where toJSON = myGenericToJSON
instance FromJSON RelatesTo where parseJSON = myGenericParseJSON


-- * Helpers

mTypeGenericParseJSON ∷ ∀a. (Bounded a, Enum a, Typeable a, ToJSON a) ⇒ Value → Parser a
mTypeGenericParseJSON jsonInput
  = maybe (typeMismatch (show . typeRep $ Proxy @a) jsonInput) pure
  $ find ((jsonInput ==) . toJSON) [minBound .. maxBound ∷ a]
