{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module MatrixBot.Bot.BotConfig
  ( BotConfig (..)
  , BotConfigReactToUsers (..)

  , BotConfigReplyToMedia (..)
  , BotConfigReplyToMedia_MessageTemplateEntry (..)
  , BotConfigReplyToMedia_DynamicFieldName (..)
  , BotConfigReplyToMedia_DynamicExtractedValueName (..)
  ) where

import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as J
import Data.Data (Proxy (Proxy))
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Typeable (Typeable, typeRep)
import GHC.Generics (Generic)
import MatrixBot.AesonUtils (myGenericToJSON, myGenericParseJSON)
import MatrixBot.Bot.EventsListener.Filters (UsersFilter, RoomsFilter, MediaMsgtypeFilter)


-- | Bot configuration
--
-- Note that feature configuration is a list of entries.
-- It means a list of filters and applied action in case a filter is matching.
-- An event will go through all such entries and apply each that would have its
-- filter matching the event.
data BotConfig = BotConfig
  { botConfigReactToUsers ∷ Maybe [BotConfigReactToUsers]
  , botConfigReplyToMedia ∷ Maybe [BotConfigReplyToMedia]
  }
  deriving stock (Generic, Eq, Show)

instance J.ToJSON BotConfig where toJSON = myGenericToJSON
instance J.FromJSON BotConfig where parseJSON = myGenericParseJSON


-- | Bot configuration entry for “react-to-users" feature
data BotConfigReactToUsers = BotConfigReactToUsers
  { botConfigReactToUsersUsersFilter ∷ Maybe UsersFilter
  , botConfigReactToUsersRoomsFilter ∷ Maybe RoomsFilter

  , botConfigReactToUsersLeaveReactions ∷ NE.NonEmpty Text
  -- ^ A list of reactions to leave for an event
  --   (a list of reactions must be non-empty,
  --   otherwise the whole section does not make sense)
  }
  deriving stock (Generic, Eq, Show)

instance J.ToJSON BotConfigReactToUsers where toJSON = myGenericToJSON
instance J.FromJSON BotConfigReactToUsers where parseJSON = myGenericParseJSON


-- | Bot configuration entry for “reply-to-media" feature
data BotConfigReplyToMedia = BotConfigReplyToMedia
  { botConfigReplyToMediaUsersFilter ∷ Maybe UsersFilter
  , botConfigReplyToMediaRoomsFilter ∷ Maybe RoomsFilter
  , botConfigReplyToMediaMsgtypeFilter ∷ Maybe MediaMsgtypeFilter

  , botConfigReplyToMediaMessageTemplate ∷ [BotConfigReplyToMedia_MessageTemplateEntry]
  -- ^ Message template, a list of entries that will be just concatenated
  , botConfigReplyToMediaHtmlMessageTemplate ∷ Maybe [BotConfigReplyToMedia_MessageTemplateEntry]
  -- ^ Optional HTML-formatted message template, to be paired with the plain text template
  }
  deriving stock (Generic, Eq, Show)

instance J.ToJSON BotConfigReplyToMedia where toJSON = myGenericToJSON
instance J.FromJSON BotConfigReplyToMedia where parseJSON = myGenericParseJSON


-- | Either a plain string or a special entry for a dynamic value substitution
data BotConfigReplyToMedia_MessageTemplateEntry
  = BotConfigReplyToMedia_MessageTemplateEntry_PlainString
      Text
  | BotConfigReplyToMedia_MessageTemplateEntry_DynamicSubstitution_Field
      BotConfigReplyToMedia_DynamicFieldName
  | BotConfigReplyToMedia_MessageTemplateEntry_DynamicSubstitution_ExtractedValue
      BotConfigReplyToMedia_DynamicExtractedValueName
  deriving stock (Generic, Eq, Show)

instance J.ToJSON BotConfigReplyToMedia_MessageTemplateEntry where
  toJSON (BotConfigReplyToMedia_MessageTemplateEntry_PlainString x) = J.toJSON x
  toJSON (BotConfigReplyToMedia_MessageTemplateEntry_DynamicSubstitution_Field x) =
    J.Object $ KeyMap.singleton "field" (J.toJSON x)
  toJSON (BotConfigReplyToMedia_MessageTemplateEntry_DynamicSubstitution_ExtractedValue x) =
    J.Object $ KeyMap.singleton "extracted_value" (J.toJSON x)

instance J.FromJSON BotConfigReplyToMedia_MessageTemplateEntry where
  parseJSON (J.String x) = pure $ BotConfigReplyToMedia_MessageTemplateEntry_PlainString x
  parseJSON jsonValue
    = flip parseJsonFromVariantsEqualityCheck jsonValue
    $ [BotConfigReplyToMedia_MessageTemplateEntry_DynamicSubstitution_Field x | x ← [minBound .. maxBound]]
    <> [BotConfigReplyToMedia_MessageTemplateEntry_DynamicSubstitution_ExtractedValue x | x ← [minBound .. maxBound]]


data BotConfigReplyToMedia_DynamicFieldName
  = BotConfigReplyToMedia_DynamicFieldName_MsgType
  -- ^ @msgtype@ — For example @m.image@
  | BotConfigReplyToMedia_DynamicFieldName_Body
  -- ^ @body@ — Contains file name
  | BotConfigReplyToMedia_DynamicFieldName_Url
  -- ^ @body@ — Contains file name
  deriving stock (Generic, Eq, Show, Bounded, Enum)

instance J.ToJSON BotConfigReplyToMedia_DynamicFieldName where
  toJSON BotConfigReplyToMedia_DynamicFieldName_MsgType = J.String "msgtype"
  toJSON BotConfigReplyToMedia_DynamicFieldName_Body = J.String "body"
  toJSON BotConfigReplyToMedia_DynamicFieldName_Url = J.String "url"

instance J.FromJSON BotConfigReplyToMedia_DynamicFieldName where
  parseJSON = parseJsonEnum


data BotConfigReplyToMedia_DynamicExtractedValueName
  = BotConfigReplyToMedia_DynamicExtractedValueName_MediaId
  -- ^ @media_id@ — Contains media file identifier, can be used to form a direct HTTP link
  deriving stock (Generic, Eq, Show, Bounded, Enum)

instance J.ToJSON BotConfigReplyToMedia_DynamicExtractedValueName where
  toJSON BotConfigReplyToMedia_DynamicExtractedValueName_MediaId = J.String "media_id"

instance J.FromJSON BotConfigReplyToMedia_DynamicExtractedValueName where
  parseJSON = parseJsonEnum


-- | Derive @FromJSON@ based on @ToJSON@ for a Bounded Enum value.
parseJsonEnum ∷ ∀a. (Typeable a, J.ToJSON a, Bounded a, Enum a) ⇒ J.Value -> J.Parser a
parseJsonEnum = parseJsonFromVariantsEqualityCheck [minBound .. maxBound ∷ a]

-- | Derive @FromJSON@ based on @ToJSON@ using passed list of variants to apply equality check
--   against.
--
-- First variant in the list passing an equality check will be the parsed value.
parseJsonFromVariantsEqualityCheck
  ∷ ∀a. (Typeable a, J.ToJSON a)
  ⇒ [a]
  → J.Value
  → J.Parser a
parseJsonFromVariantsEqualityCheck variants jsonValue =
  case find ((jsonValue ==) . J.toJSON) variants of
    Just x → pure x
    Nothing →
      fail $ mconcat
        [ "Failed to parse ", (show . typeRep) (Proxy @a), " "
        , "(unexpected value: ", show jsonValue, ")"
        ]
