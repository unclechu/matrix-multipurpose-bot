{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module MatrixBot.Bot.Jobs.Handlers.SendReaction
  ( sendReaction
  ) where

import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON), decode)
import Data.Data (Proxy (Proxy))
import Data.Text (Text, pack)
import MatrixBot.AesonUtils (myGenericToJSON, myGenericParseJSON)
import MatrixBot.Bot.Jobs.Log (logEventResponse)
import qualified MatrixBot.Log as L
import qualified MatrixBot.MatrixApi as Api
import qualified MatrixBot.MatrixApi.Client as Api
import qualified MatrixBot.MatrixApi.Types.MEventTypes as Api
import qualified MatrixBot.SharedTypes as T
import Servant.API (AuthProtect)
import Servant.Client.Core (AuthenticatedRequest)
import qualified Servant.Client.Core as Servant
import qualified Network.HTTP.Types.Status as Http
import GHC.Generics (Generic)
import qualified Control.Exception.Safe as E
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Logger as ML


-- | Leave a reaction for a Matrix room event
sendReaction
  ∷ (MonadIO m, MonadFail m, E.MonadCatch m, ML.MonadLogger m)
  ⇒ Api.MatrixApiClient
  → AuthenticatedRequest (AuthProtect "access-token")
  → T.TransactionId
  → T.RoomId
  → T.EventId
  → Text
  → m (Maybe Api.EventResponse)
  -- ^ @Nothing@ in case of “same reaction twice error” (idempotency)
sendReaction req auth transactionId roomId eventId reactionText = do
  L.logDebug $ mconcat
    [ "Sending reaction ", pack . show $ reactionText
    , " to room ", pack . show . T.printRoomId $ roomId
    , " for ", T.unEventId eventId
    , "…"
    ]

  response ← do
    let proxy = Proxy @(Api.SendEventApi (Api.MEventTypeOneOf '[ 'Api.MReactionType ])) in
      ignoreSendSameReactionTwiceError $
        Api.runMatrixApiClient' req proxy $ \f → f
          auth
          roomId
          Api.MReactionTypeOneOf
          transactionId
          (Api.MReactionContent $ Api.RelatesTo eventId reactionText Api.MAnnotationTypeOneOf)

  response <$ case response of
    Just x → logEventResponse x
    Nothing → L.logDebug $ mconcat
      [ "Reaction ", pack . show $ reactionText
      , " to room ", pack . show . T.printRoomId $ roomId
      , " for ", T.unEventId eventId
      , " is already sent (considering this a success)"
      ]


-- | A helper to guarantee idempotency of applying a reaction.
--
-- If reaction is already present the error will be just ignored.
ignoreSendSameReactionTwiceError
  ∷ (E.MonadCatch m, ML.MonadLogger m)
  ⇒ m a
  → m (Maybe a)
  -- ^ @Nothing@ in case it’s “same reaction twice” ignored error
ignoreSendSameReactionTwiceError =
  flip E.catch ((Nothing <$) . resolveClientError) . fmap Just
  where
    logCaughtException ∷ (Show e, ML.MonadLogger m) ⇒ e → m ()
    logCaughtException e =
      L.logWarn $ "Caught this error (just ignoring it): " <> (pack . show) e

    resolveClientError ∷ (E.MonadThrow m, ML.MonadLogger m) ⇒ Servant.ClientError → m ()
    resolveClientError e =
      maybe (E.throwM e) logCaughtException $ do
        response ← case e of
          Servant.FailureResponse _req response → pure response
          _ → Nothing
        guard $ (Http.statusCode . Servant.responseStatusCode) response == 400
        x ← decode @ErrorResponse . Servant.responseBody $ response
        guard $ errorResponseErrcode x == "M_UNKNOWN"
        guard $ errorResponseError x == "Can't send same reaction twice"
        pure x


data ErrorResponse = ErrorResponse
  { errorResponseErrcode ∷ Text
  , errorResponseError ∷ Text
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON ErrorResponse where toJSON = myGenericToJSON
instance FromJSON ErrorResponse where parseJSON = myGenericParseJSON
