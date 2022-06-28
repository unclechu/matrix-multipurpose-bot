{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ConstraintKinds #-}

module MatrixBot.MatrixApi.Types.MEventTypes
     ( MEventType (..)
     , MEventTypeOneOf (..)
     , mEventTypeToString
     , mEventTypeOneOfToMEventType
     ) where

import GHC.TypeLits

import Data.Function (on)
import Data.List (find)
import Data.Proxy
import Data.String (IsString (fromString))
import Data.Typeable
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Servant.API (ToHttpApiData (..))


-- | Matrix event types enum
data MEventType
  = MAnnotationType
  | MReactionType
  | MRoomMessageType
  | MLoginPasswordType
  deriving stock (Show, Eq, Enum, Bounded)

type family MEventTypeToString (a ∷ MEventType) where
  MEventTypeToString 'MAnnotationType = "m.annotation"
  MEventTypeToString 'MReactionType = "m.reaction"
  MEventTypeToString 'MRoomMessageType = "m.room.message"
  MEventTypeToString 'MLoginPasswordType = "m.login.password"

mEventTypeToString ∷ IsString s ⇒ MEventType → s
mEventTypeToString = fromString . \case
  MAnnotationType → symbolVal $ Proxy @(MEventTypeToString 'MAnnotationType)
  MReactionType → symbolVal $ Proxy @(MEventTypeToString 'MReactionType)
  MRoomMessageType → symbolVal $ Proxy @(MEventTypeToString 'MRoomMessageType)
  MLoginPasswordType → symbolVal $ Proxy @(MEventTypeToString 'MLoginPasswordType)

-- | Limited/restricted subset of "MEventType"
data MEventTypeOneOf (types ∷ [MEventType]) where
  MAnnotationTypeOneOf ∷ OneOf 'MAnnotationType types ~ 'True ⇒ MEventTypeOneOf types
  MReactionTypeOneOf ∷ OneOf 'MReactionType types ~ 'True ⇒ MEventTypeOneOf types
  MRoomMessageTypeOneOf ∷ OneOf 'MRoomMessageType types ~ 'True ⇒ MEventTypeOneOf types
  MLoginPasswordTypeOneOf ∷ OneOf 'MLoginPasswordType types ~ 'True ⇒ MEventTypeOneOf types

mEventTypeOneOfToMEventType ∷ MEventTypeOneOf (t ': ts) → MEventType
mEventTypeOneOfToMEventType = \case
  MAnnotationTypeOneOf → MAnnotationType
  MReactionTypeOneOf → MReactionType
  MRoomMessageTypeOneOf → MRoomMessageType
  MLoginPasswordTypeOneOf → MLoginPasswordType

instance (ItemParser t ts to x xs, t ~ 'MAnnotationType) ⇒ ToParser '(to, 'MAnnotationType ': ts) where
  toParser Proxy = genericToParser (Proxy @'(to, t ': ts)) MAnnotationTypeOneOf
instance (ItemParser t ts to x xs, t ~ 'MReactionType) ⇒ ToParser '(to, 'MReactionType ': ts) where
  toParser Proxy = genericToParser (Proxy @'(to, t ': ts)) MReactionTypeOneOf
instance (ItemParser t ts to x xs, t ~ 'MRoomMessageType) ⇒ ToParser '(to, 'MRoomMessageType ': ts) where
  toParser Proxy = genericToParser (Proxy @'(to, t ': ts)) MRoomMessageTypeOneOf
instance (ItemParser t ts to x xs, t ~ 'MLoginPasswordType) ⇒ ToParser '(to, 'MLoginPasswordType ': ts) where
  toParser Proxy = genericToParser (Proxy @'(to, t ': ts)) MLoginPasswordTypeOneOf


instance Aeson.ToJSON MEventType where
  toJSON = Aeson.String . mEventTypeToString

instance Aeson.FromJSON MEventType where
  parseJSON ∷ ∀a. (a ~ MEventType) ⇒ Aeson.Value → Aeson.Parser a
  parseJSON jsonInput
    = maybe (Aeson.typeMismatch (show . typeRep $ Proxy @a) jsonInput) pure
    $ find ((jsonInput ==) . Aeson.toJSON) [minBound .. maxBound ∷ a]


-- Matching non-empty list (it’s impossible to have any value of an empty list)
instance Show (MEventTypeOneOf (t ': ts)) where
  show = show . mEventTypeOneOfToMEventType

instance Eq (MEventTypeOneOf (t ': ts)) where
  (==) = (==) `on` mEventTypeOneOfToMEventType

instance ToHttpApiData (MEventTypeOneOf (t ': ts)) where
  toUrlPiece = mEventTypeToString . mEventTypeOneOfToMEventType

instance Aeson.ToJSON (MEventTypeOneOf (t ': ts)) where
  toJSON = Aeson.toJSON . mEventTypeOneOfToMEventType

instance ToParser '(t ': ts, t ': ts) ⇒ Aeson.FromJSON (MEventTypeOneOf (t ': ts)) where
  parseJSON = toParser (Proxy @'(t ': ts, t ': ts))


-- | Serialize a list of "MEventType"s to a JSON parser
class ToParser (a ∷ ([MEventType], [MEventType])) where
  toParser ∷ Proxy a → Aeson.Value → Aeson.Parser (MEventTypeOneOf (Fst a))

instance ToParser '(x ': xs, '[]) where
  toParser Proxy = Aeson.typeMismatch "MEventTypeOneOf"

type ItemParser t ts to x xs = (to ~ x ': xs, OneOf t to ~ 'True, ToParser '(to, ts))

genericToParser
  ∷ ∀ t ts to x xs
  . ItemParser t ts to x xs
  ⇒ Proxy '(to, t ': ts)
  → MEventTypeOneOf to
  → Aeson.Value
  → Aeson.Parser (MEventTypeOneOf to)
genericToParser Proxy x j
  | j == Aeson.toJSON (mEventTypeOneOfToMEventType x) = pure x
  | otherwise = toParser (Proxy @'(to, ts)) j


-- * Helpers

type family OneOf (x ∷ k) (xs ∷ [k]) ∷ Bool where
  OneOf _ '[] = 'False
  OneOf x (x ': _) = 'True
  OneOf x (y ': ys) = OneOf x ys

type family Fst (a ∷ (k1, k2)) ∷ k1 where
  Fst '(a, _) = a
