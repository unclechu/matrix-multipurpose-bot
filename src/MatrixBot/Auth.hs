{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DataKinds #-}

-- | Authentication-related stuff
module MatrixBot.Auth where

import GHC.Generics

import Data.Proxy
import Data.Text (pack)
import Data.Aeson (ToJSON (..), FromJSON (..))

import Control.Exception.Safe (MonadThrow)
import Control.Lens (Lens', view)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
import qualified Control.Monad.Logger as ML

import Servant.API (AuthProtect)
import Servant.Client
import Servant.Client.Core (addHeader)
import Servant.Client.Core.Auth

import MatrixBot.AesonUtils (myGenericToJSON, myGenericParseJSON)
import MatrixBot.Log
import qualified MatrixBot.MatrixApi as Api
import qualified MatrixBot.MatrixApi.Client as Api
import qualified MatrixBot.SharedTypes as T


-- * Functions

authenticate
  ∷ (MonadIO m, MonadUnliftIO m, MonadThrow m, ML.MonadLogger m)
  ⇒ T.Mxid
  → T.Password
  → m Credentials
authenticate mxid password = do
  logDebug $
    "Creating request handler for "
    <> (pack . show . T.unHomeServer . T.mxidHomeServer) mxid <> "…"

  req ← Api.mkMatrixApiClient Api.defaultRequestOptions . T.mxidHomeServer $ mxid

  logDebug $ "Authenticating as " <> (pack . show . T.printMxid) mxid <> "…"

  response
    ← Api.runMatrixApiClient' req
    $ client @Api.LoginApi Proxy $ Api.LoginRequest
    { Api.loginRequestType = Api.MLoginPasswordType
    , Api.loginRequestUser = T.mxidUsername mxid
    , Api.loginRequestPassword = password
    }

  pure Credentials
    { credentialsUsername = T.mxidUsername . Api.loginResponseUserId $ response
    , credentialsHomeServer = Api.loginResponseHomeServer response
    , credentialsAccessToken = Api.loginResponseAccessToken response
    }


getAuthenticatedMatrixRequest
  ∷ (MonadReader r m, HasCredentials r)
  ⇒ m (AuthenticatedRequest (AuthProtect "access-token"))
getAuthenticatedMatrixRequest = do
  token ← asks $ credentialsAccessToken . view credentials
  pure . mkAuthenticatedRequest token $ addHeader "Authorization" . ("Bearer " <>) . T.unAccessToken


-- * Data types

-- | Set of credentials used for authentication
data Credentials = Credentials
  { credentialsUsername ∷ T.Username
  , credentialsHomeServer ∷ T.HomeServer
  , credentialsAccessToken ∷ T.AccessToken
  }
  deriving stock (Generic, Eq, Show)

instance ToJSON Credentials where toJSON = myGenericToJSON
instance FromJSON Credentials where parseJSON = myGenericParseJSON


class HasCredentials r where
  credentials ∷ Lens' r Credentials

instance HasCredentials Credentials where
  credentials = id
