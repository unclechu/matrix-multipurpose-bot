{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module MatrixBot.Bot.BotConfig
  ( BotConfig (..)
  , BotConfigReactToUsers (..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Types (FromJSON (parseJSON))
import qualified Data.List.NonEmpty as NE
import qualified MatrixBot.SharedTypes as T
import Data.Text (Text)
import MatrixBot.AesonUtils (myGenericToJSON, myGenericParseJSON)


-- | Bot configuration
newtype BotConfig = BotConfig
  { botConfigReactToUsers ∷ Maybe [BotConfigReactToUsers]
  }
  deriving stock (Generic, Eq, Show)

instance ToJSON BotConfig where toJSON = myGenericToJSON
instance FromJSON BotConfig where parseJSON = myGenericParseJSON


-- | Bot configuration for “react-to-users" feature
data BotConfigReactToUsers = BotConfigReactToUsers
  { botConfigReactToUsersUsersFilter ∷ Maybe (NE.NonEmpty T.Mxid)
  , botConfigReactToUsersRoomsFilter ∷ Maybe (NE.NonEmpty T.RoomId)

  -- A list of reactions must be non-empty,
  -- otherwise the whole section does not make sense.
  , botConfigReactToUsersLeaveReactions ∷ NE.NonEmpty Text
  }
  deriving stock (Generic, Eq, Show)

instance ToJSON BotConfigReactToUsers where toJSON = myGenericToJSON
instance FromJSON BotConfigReactToUsers where parseJSON = myGenericParseJSON
