{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | A set of generic types that can be used in different places/modules
module MatrixBot.SharedTypes where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Proxy
import Data.Text (Text)
import Data.Typeable
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.Attoparsec.Text as AP

import Control.Monad.IO.Class

import Servant.API


newtype Username = Username { unUsername ∷ Text }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)


newtype Password = Password { unPassword ∷ Text }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)


newtype AccessToken = AccessToken { unAccessToken ∷ Text }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)


newtype HomeServer = HomeServer { unHomeServer ∷ Text }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)


-- | Matrix user identifier aka MXID
--
-- Looks like this:
--
-- @
-- @username:homeserver.tld
-- @
data Mxid = Mxid
  { mxidUsername ∷ Username
  , mxidHomeServer ∷ HomeServer
  }
  deriving stock (Eq, Show, Typeable)

instance ToJSON Mxid where
  toJSON = String . printMxid

instance FromJSON Mxid where
  parseJSON ∷ ∀a. (a ~ Mxid) ⇒ Value → Parser a
  parseJSON jsonInput =
    case jsonInput of
      String x → either fail pure $ AP.parseOnly mxidParser x
      _ → failure
    where
      failure = typeMismatch typeName jsonInput
      typeName = show . typeRep $ Proxy @a

instance ToHttpApiData Mxid where
  toUrlPiece = printMxid

mxidParser ∷ AP.Parser Mxid
mxidParser = Mxid
  <$> fmap Username ("@" *> AP.takeTill (== ':') >>= failOnEmpty "username")
  <*> fmap HomeServer (":" *> AP.takeText >>= failOnEmpty "home server")
  <* AP.endOfInput
  where
    failOnEmpty name x
      | x /= mempty = pure x
      | otherwise = fail $ "Empty " <> name

printMxid ∷ Mxid → Text
printMxid mxid = mconcat
  [ "@", unUsername . mxidUsername $ mxid
  , ":", unHomeServer . mxidHomeServer $ mxid
  ]


-- | Matrix room identifier
--
-- Looks like this:
--
-- @
-- !ffffffffffffffffff:homeserver.tld
-- @
data RoomId = RoomId
  { roomIdId ∷ Text
  , roomIdHomeServer ∷ HomeServer
  }
  deriving stock (Eq, Show, Typeable)

instance ToJSON RoomId where
  toJSON = String . printRoomId

instance FromJSON RoomId where
  parseJSON ∷ ∀a. (a ~ RoomId) ⇒ Value → Parser a
  parseJSON jsonInput =
    case jsonInput of
      String x → either fail pure $ AP.parseOnly roomIdParser x
      _ → failure
    where
      failure = typeMismatch typeName jsonInput
      typeName = show . typeRep $ Proxy @a

instance ToHttpApiData RoomId where
  toUrlPiece = printRoomId

roomIdParser ∷ AP.Parser RoomId
roomIdParser = RoomId
  <$> ("!" *> AP.takeTill (== ':') >>= failOnEmpty "id")
  <*> fmap HomeServer (":" *> AP.takeText >>= failOnEmpty "home server")
  <* AP.endOfInput
  where
    failOnEmpty name x
      | x /= mempty = pure x
      | otherwise = fail $ "Empty " <> name

printRoomId ∷ RoomId → Text
printRoomId roomId = mconcat
  [ "!", roomIdId roomId
  , ":", unHomeServer . roomIdHomeServer $ roomId
  ]


newtype EventId = EventId { unEventId ∷ Text }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)


newtype EventToken = EventToken { unEventToken ∷ Text }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)


newtype TransactionId = TransactionId { unTransactionId ∷ UUID }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)

genTransactionId ∷ MonadIO m ⇒ m TransactionId
genTransactionId = TransactionId <$> liftIO nextRandom


-- * Time-related stuff

newtype Milliseconds = Milliseconds { unMilliseconds ∷ Int }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)


newtype Seconds = Seconds { unSeconds ∷ Int }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)


newtype Microseconds = Microseconds { unMicroseconds ∷ Int }
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)


secondsToMilliseconds ∷ Seconds → Milliseconds
secondsToMilliseconds (Seconds x) = Milliseconds $ x * 1000

millisecondsToMicroseconds ∷ Milliseconds → Microseconds
millisecondsToMicroseconds (Milliseconds x) = Microseconds $ x * 1000

secondsToMicroseconds ∷ Seconds → Microseconds
secondsToMicroseconds = millisecondsToMicroseconds . secondsToMilliseconds
