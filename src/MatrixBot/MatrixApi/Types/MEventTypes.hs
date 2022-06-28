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

import Data.Foldable (foldl')
import Data.Function (on)
import Data.List (find)
import Data.Proxy
import Data.String (IsString (fromString))
import Data.Typeable
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Control.Applicative ((<|>), Alternative (empty))

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

class DescendMEventType (a ∷ MEventType) where descendMEventType ∷ Proxy a → MEventType
instance DescendMEventType 'MAnnotationType where descendMEventType Proxy = MAnnotationType
instance DescendMEventType 'MReactionType where descendMEventType Proxy = MReactionType
instance DescendMEventType 'MRoomMessageType where descendMEventType Proxy = MRoomMessageType
instance DescendMEventType 'MLoginPasswordType where descendMEventType Proxy = MLoginPasswordType

mEventTypeToString ∷ IsString s ⇒ MEventType → s
mEventTypeToString = fromString . \case
  MAnnotationType → symbolVal $ Proxy @(MEventTypeToString 'MAnnotationType)
  MReactionType → symbolVal $ Proxy @(MEventTypeToString 'MReactionType)
  MRoomMessageType → symbolVal $ Proxy @(MEventTypeToString 'MRoomMessageType)
  MLoginPasswordType → symbolVal $ Proxy @(MEventTypeToString 'MLoginPasswordType)

instance Aeson.ToJSON MEventType where
  toJSON = Aeson.String . mEventTypeToString

instance Aeson.FromJSON MEventType where
  parseJSON ∷ ∀a. (a ~ MEventType) ⇒ Aeson.Value → Aeson.Parser a
  parseJSON jsonInput
    = maybe (Aeson.typeMismatch (show . typeRep $ Proxy @a) jsonInput) pure
    $ find ((jsonInput ==) . Aeson.toJSON) [minBound .. maxBound ∷ a]


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


-- Matching non-empty list (it’s impossible to have any value of an empty list)
instance Show (MEventTypeOneOf (t ': ts)) where
  show = show . mEventTypeOneOfToMEventType

instance Eq (MEventTypeOneOf (t ': ts)) where
  (==) = (==) `on` mEventTypeOneOfToMEventType

instance ToHttpApiData (MEventTypeOneOf (t ': ts)) where
  toUrlPiece = mEventTypeToString . mEventTypeOneOfToMEventType

instance Aeson.ToJSON (MEventTypeOneOf (t ': ts)) where
  toJSON = Aeson.toJSON . mEventTypeOneOfToMEventType

-- FIXME For some reason this instance fails with:
--   • Couldn't match type ‘'MLoginPasswordType’ with ‘'MReactionType’
--       arising from a use of ‘myGenericParseJSON’
{-instance (IsEmpty a ~ 'False, MEventTypesToParsers a (ListDup a)) ⇒ Aeson.FromJSON (MEventTypeOneOf a) where
  parseJSON jsonInput
    = foldl' (\acc f → f jsonInput <|> acc) (Aeson.typeMismatch "MEventTypeOneOf" jsonInput)
    $ mEventTypesToParsers (Proxy @'(a, ListDup a))-}
instance Aeson.FromJSON (MEventTypeOneOf '[ 'MLoginPasswordType ]) where
  parseJSON ∷ ∀t. t ~ 'MLoginPasswordType ⇒ Aeson.Value → Aeson.Parser (MEventTypeOneOf '[t])
  parseJSON j
    | j == Aeson.toJSON (descendMEventType @t Proxy) = pure MLoginPasswordTypeOneOf
    | otherwise = empty
instance Aeson.FromJSON (MEventTypeOneOf '[ 'MRoomMessageType ]) where
  parseJSON ∷ ∀t. t ~ 'MRoomMessageType ⇒ Aeson.Value → Aeson.Parser (MEventTypeOneOf '[t])
  parseJSON j
    | j == Aeson.toJSON (descendMEventType @'MRoomMessageType Proxy) = pure MRoomMessageTypeOneOf
    | otherwise = empty
instance Aeson.FromJSON (MEventTypeOneOf '[ 'MAnnotationType ]) where
  parseJSON ∷ ∀t. t ~ 'MAnnotationType ⇒ Aeson.Value → Aeson.Parser (MEventTypeOneOf '[t])
  parseJSON j
    | j == Aeson.toJSON (descendMEventType @'MAnnotationType Proxy) = pure MAnnotationTypeOneOf
    | otherwise = empty


{-class MEventTypesToParsers (typesToParse ∷ [MEventType]) (types ∷ [(MEventType, MEventType)]) where
  mEventTypesToParsers
    ∷ Proxy '(typesToParse, types)
    → [Aeson.Value → Aeson.Parser (MEventTypeOneOf typesToParse)]

instance MEventTypesToParsers (t ': ts) '[] where
  mEventTypesToParsers Proxy = []

instance
  (MEventTypesToParsersItem t ts types s, t ~ 'MAnnotationType)
  ⇒ MEventTypesToParsers types ('(t, 'MAnnotationType) ': ts)
  where
  mEventTypesToParsers Proxy = mEventTypesToParsersGeneric (Proxy @'(t, ts, types, s)) MAnnotationTypeOneOf

instance
  (MEventTypesToParsersItem t ts types s, t ~ 'MReactionType)
  ⇒ MEventTypesToParsers types ('(t, 'MReactionType) ': ts)
  where
  mEventTypesToParsers Proxy = mEventTypesToParsersGeneric (Proxy @'(t, ts, types, s)) MReactionTypeOneOf

instance
  (MEventTypesToParsersItem t ts types s, t ~ 'MRoomMessageType)
  ⇒ MEventTypesToParsers types ('(t, 'MRoomMessageType) ': ts)
  where
  mEventTypesToParsers Proxy = mEventTypesToParsersGeneric (Proxy @'(t, ts, types, s)) MRoomMessageTypeOneOf

instance
  (MEventTypesToParsersItem t ts types s, t ~ 'MLoginPasswordType)
  ⇒ MEventTypesToParsers types ('(t, 'MLoginPasswordType) ': ts)
  where
  mEventTypesToParsers Proxy = mEventTypesToParsersGeneric (Proxy @'(t, ts, types, s)) MLoginPasswordTypeOneOf

type MEventTypesToParsersItem t ts types s =
  ( t ~ 'MReactionType
  , s ~ MEventTypeToString t
  , OneOf t types ~ 'True
  , IsEmpty types ~ 'False
  , MEventTypesToParsers types ts
  )

mEventTypesToParsersGeneric
  ∷ ∀ t ts types s . MEventTypesToParsersItem t ts types s
  ⇒ Proxy '(t, ts, types, s)
  → MEventTypeOneOf types
  → [Aeson.Value → Aeson.Parser (MEventTypeOneOf types)]
mEventTypesToParsersGeneric Proxy x
  = (\j → if j == Aeson.String (fromString . symbolVal $ Proxy @s) then pure x else empty)
  : mEventTypesToParsers (Proxy @'(types, ts))-}


-- * Helpers

type family OneOf (x ∷ k) (xs ∷ [k]) ∷ Bool where
  OneOf _ '[] = 'False
  OneOf x (x ': _) = 'True
  OneOf x (y ': ys) = OneOf x ys

type family IsEmpty (a ∷ [k]) ∷ Bool where
  IsEmpty '[] = 'True
  IsEmpty (_ ': _) = 'False

type family ListDup (a ∷ [k]) ∷ [(k, k)] where
  ListDup '[] = '[]
  ListDup (x ': xs) = '(x, x) ': ListDup xs
