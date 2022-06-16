{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module MatrixBot.AesonUtils where

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char
import Data.Function
import Data.Typeable

import Control.Category ((>>>))


myGenericToJSON ∷ ∀a. (Generic a, Typeable a, GToJSON' Value Zero (Rep a)) ⇒ a → Value
myGenericToJSON
  = genericToJSON
  $ defaultOptions
  & cutTypeNamePrefix @a Proxy
  & camelCaseToSnakeCase


myGenericParseJSON ∷ ∀a. (Generic a, Typeable a, GFromJSON Zero (Rep a)) ⇒ Value → Parser a
myGenericParseJSON
  = genericParseJSON
  $ defaultOptions
  & cutTypeNamePrefix @a Proxy
  & camelCaseToSnakeCase


-- | Cut type name prefix from each field
--
-- For instance if type is called “Foo” and its fields are “fooBar” and “fooBaz” then its fields
-- will be named “bar” and “baz”. If prefix doesn’t equal to “foo” (to the type name) then the field
-- name stays unchanged.
cutTypeNamePrefix ∷ ∀a. Typeable a ⇒ Proxy a → Options → Options
cutTypeNamePrefix Proxy opts = opts
  { fieldLabelModifier = fieldLabelModifier opts >>> \x →
      let
        typeNamePrefix
          = cutTypeParams
          $ case show . typeRep $ Proxy @a of
              a : as → toLower a : as
              a → a

        cutTypeParams "" = ""
        cutTypeParams (' ' : _) = ""
        cutTypeParams (c : cs) = c : cutTypeParams cs
      in
        case splitAt (length typeNamePrefix) x of
          (a, b : bs) | a == typeNamePrefix → toLower b : bs
          _ → x
  }


-- | Convert “camelCase” to “snake_case” for each field
camelCaseToSnakeCase ∷ Options → Options
camelCaseToSnakeCase opts = opts
  { fieldLabelModifier = (fieldLabelModifier opts >>>) $ foldMap $ \case
      c | isUpper c → ['_', toLower c]
        | otherwise → pure c
  }
