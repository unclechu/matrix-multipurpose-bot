{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module MatrixBot.MatrixApi.Client where

import Data.Text (unpack)

import Control.Exception.Safe (MonadThrow, throwM)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import qualified Control.Monad.Logger as ML

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

import Servant.API
import Servant.Client
import Servant.Client.Core.Auth

import qualified MatrixBot.SharedTypes as T


data MatrixApiClient = MatrixApiClient
  { runMatrixApiClient ∷ ∀m a. MonadIO m ⇒ ClientM a → m (Either ClientError a)
  , runMatrixApiClient' ∷ ∀m a. (MonadIO m, MonadThrow m) ⇒ ClientM a → m a
  }


data RequestOptions = RequestOptions
  { requestOptionsTimeout ∷ Maybe T.Seconds
  -- ^ "Nothing" means default timeout value (30 seconds)
  , requestOptionsRequestLogger ∷ ∀m. ML.MonadLogger m ⇒ HTTP.Request → m ()
  }


defaultRequestOptions ∷ RequestOptions
defaultRequestOptions = RequestOptions
  { requestOptionsTimeout = Nothing
  , requestOptionsRequestLogger = \_req → pure ()
  }


mkMatrixApiClient
  ∷ (MonadIO m, MonadUnliftIO m, MonadThrow m, ML.MonadLogger m)
  ⇒ RequestOptions
  → T.HomeServer
  → m MatrixApiClient
mkMatrixApiClient reqOpts homeServer = do
  tlsManager ← withRunInIO $ \runInIO → do
    let
      managerSettings = HTTP.defaultManagerSettings
        { HTTP.managerResponseTimeout = responseTimeout
        , HTTP.managerModifyRequest = \req → do
            runInIO $ requestOptionsRequestLogger reqOpts req
            HTTP.managerModifyRequest HTTP.defaultManagerSettings req
        }
    HTTP.newTlsManagerWith managerSettings

  baseUrl' ← parseBaseUrl . unpack . T.unHomeServer $ homeServer

  let
    clientEnv = mkClientEnv tlsManager baseUrl'

    f ∷ MonadIO m ⇒ ClientM a → m (Either ClientError a)
    f = liftIO . flip runClientM clientEnv

  pure $ MatrixApiClient f (f >=> either throwM pure)

  where
    responseTimeout =
      maybe
        HTTP.responseTimeoutDefault
        (HTTP.responseTimeoutMicro . fromInteger . T.unMicroseconds . T.secondsToMicroseconds)
        (requestOptionsTimeout reqOpts)


type instance AuthClientData (AuthProtect "access-token") = T.AccessToken
