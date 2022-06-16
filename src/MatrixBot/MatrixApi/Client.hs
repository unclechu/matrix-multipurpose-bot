{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module MatrixBot.MatrixApi.Client where

import Data.Text (unpack)

import Control.Exception.Safe (MonadThrow, throwM)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))

import Network.HTTP.Client.TLS (newTlsManager)

import Servant.API
import Servant.Client
import Servant.Client.Core.Auth

import qualified MatrixBot.SharedTypes as T


data MatrixApiClient = MatrixApiClient
  { runMatrixApiClient ∷ ∀m a. MonadIO m ⇒ ClientM a → m (Either ClientError a)
  , runMatrixApiClient' ∷ ∀m a. (MonadIO m, MonadThrow m) ⇒ ClientM a → m a
  }


mkMatrixApiClient ∷ (MonadIO m, MonadThrow m) ⇒ T.HomeServer → m MatrixApiClient
mkMatrixApiClient homeServer = do
  tlsManager ← newTlsManager
  baseUrl' ← parseBaseUrl . unpack . T.unHomeServer $ homeServer

  let
    clientEnv = mkClientEnv tlsManager baseUrl'

    f ∷ MonadIO m ⇒ ClientM a → m (Either ClientError a)
    f = liftIO . flip runClientM clientEnv

  pure $ MatrixApiClient f (f >=> either throwM pure)


type instance AuthClientData (AuthProtect "access-token") = T.AccessToken
