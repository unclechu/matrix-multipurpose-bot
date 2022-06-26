{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module MatrixBot.MatrixApi.Client where

import Data.Proxy
import Data.Text (pack, unpack)

import Control.Exception.Safe (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Control.Monad.Free as Free
import qualified Control.Monad.Logger as ML

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

import Servant.API
import Servant.Client
import Servant.Client.Core.Auth
import qualified Servant.Client.Free as ServantFree

import MatrixBot.Log (logDebug)
import qualified MatrixBot.SharedTypes as T


data MatrixApiClient = MatrixApiClient
  { runMatrixApiClient
      ∷ ∀ api m a .
      ( MonadIO m
      , MonadFail m -- Resolving unexpected cases to a failure
      , Show a -- Paired with @MonadFail@ to print unexpected values
      , ML.MonadLogger m -- Logging the request
      , HasClient (Free.Free ServantFree.ClientF) api -- For logging requests
      , HasClient ClientM api
      )
      ⇒ Proxy api
      → (∀clientM. HasClient clientM api ⇒ Client clientM api → clientM a)
      → m (Either ClientError a)
  , runMatrixApiClient'
      ∷ ∀ api m a .
      ( MonadIO m
      , MonadFail m -- Resolving unexpected cases to a failure
      , Show a -- Paired with @MonadFail@ to print unexpected values
      , MonadThrow m -- Throwing error instead of returning @Either ClientError@
      , ML.MonadLogger m -- Logging the request
      , HasClient (Free.Free ServantFree.ClientF) api -- For logging requests
      , HasClient ClientM api
      )
      ⇒ Proxy api
      → (∀clientM. HasClient clientM api ⇒ Client clientM api → clientM a)
      → m a
  }


newtype RequestOptions = RequestOptions
  { requestOptionsTimeout ∷ Maybe T.Seconds
  -- ^ "Nothing" means default timeout value (30 seconds)
  }


defaultRequestOptions ∷ RequestOptions
defaultRequestOptions = RequestOptions
  { requestOptionsTimeout = Nothing
  }


mkMatrixApiClient
  ∷ (MonadIO m, MonadThrow m, ML.MonadLogger m)
  ⇒ RequestOptions
  → T.HomeServer
  → m MatrixApiClient
mkMatrixApiClient reqOpts homeServer = do
  tlsManager ←
    HTTP.newTlsManagerWith HTTP.tlsManagerSettings { HTTP.managerResponseTimeout = responseTimeout }

  baseUrl' ← parseBaseUrl . unpack . ("https://" <>) . T.unHomeServer $ homeServer

  let
    clientEnv = mkClientEnv tlsManager baseUrl'

    f
      ∷ ∀ api m a .
      ( MonadIO m
      , MonadFail m
      , ML.MonadLogger m
      , Show a
      , HasClient (Free.Free ServantFree.ClientF) api
      , HasClient ClientM api
      )
      ⇒ Proxy api
      → (∀clientM. HasClient clientM api ⇒ Client clientM api → clientM a)
      → m (Either ClientError a)
    f p@Proxy genericClientF = do
      case genericClientF . ServantFree.client $ p of
        Free.Free (ServantFree.RunRequest req _responseResolver) →
          logDebug $ mconcat
            [ "Making a client request: "
            , pack . show . defaultMakeClientRequest baseUrl' $ req
            ]
        Free.Free (ServantFree.Throw x) →
          fail $ "Unexpected client request mock failure: " <> show x
        Free.Pure x →
          fail $ "Unexpected client request mock Pure: " <> show x

      liftIO . flip runClientM clientEnv . genericClientF . client $ p

  pure $ MatrixApiClient f (\p clientF → f p clientF >>= either throwM pure)

  where
    responseTimeout =
      maybe
        HTTP.responseTimeoutDefault
        (HTTP.responseTimeoutMicro . fromInteger . T.unMicroseconds . T.secondsToMicroseconds)
        (requestOptionsTimeout reqOpts)


type instance AuthClientData (AuthProtect "access-token") = T.AccessToken
