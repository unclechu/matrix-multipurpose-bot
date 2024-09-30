{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- | Events filtering implementation
module MatrixBot.Bot.EventsListener.Filters
  ( UsersFilter
  , filterByUser

  , RoomsFilter
  , filterByRoom

  , MediaMsgtypeFilter
  , MediaMsgtype (..)
  , filterByMediaMsgtype
  ) where

import qualified Control.Monad.Trans.Except as Except
import Data.Text (Text, pack)
import qualified MatrixBot.SharedTypes as T
import qualified Data.List.NonEmpty as NE
import qualified MatrixBot.Log as L
import qualified Control.Monad.Logger as ML
import qualified MatrixBot.MatrixApi as Api
import GHC.Generics (Generic)
import qualified Data.Aeson as J
import Data.List (find)


type UsersFilter = NE.NonEmpty T.Mxid

filterByUser
  ∷ ML.MonadLogger m
  ⇒ Maybe UsersFilter
  -- ^ Whitelisted user IDs
  → T.Mxid
  -- ^ Current user ID
  → Except.ExceptT Text m ()
filterByUser usersFilter userId =
  case usersFilter of
    Nothing →
      L.logDebug $ mconcat
        [ "There is no user filter"
        , " (filter passed)"
        ]
    Just x | userId `elem` x →
      L.logDebug $ mconcat
        [ "User ", T.printMxid userId
        , " is one of these: ", (pack . show) x
        , " (filter passed)"
        ]
    Just x →
      Except.throwE $ mconcat
        [ "User ", T.printMxid userId
        , " is not one of these: ", (pack . show) x
        , " (filter not passed)"
        ]


type RoomsFilter = NE.NonEmpty T.RoomId

filterByRoom
  ∷ ML.MonadLogger m
  ⇒ Maybe RoomsFilter
  -- ^ Whitelisted room IDs
  → T.RoomId
  -- ^ Current room ID
  → Except.ExceptT Text m ()
filterByRoom roomsFilter roomId =
  case roomsFilter of
    Nothing →
      L.logDebug $ mconcat
        [ "There is no room filter"
        , " (filter passed)"
        ]
    Just x | roomId `elem` x →
      L.logDebug $ mconcat
        [ "Room ", T.printRoomId roomId
        , " is one of these: ", (pack . show) x
        , " (filter passed)"
        ]
    Just x →
      Except.throwE $ mconcat
        [ "Room ", T.printRoomId roomId
        , " is not one of these: ", (pack . show) x
        , " (filter not passed)"
        ]


type MediaMsgtypeFilter = NE.NonEmpty MediaMsgtype

data MediaMsgtype
  = MediaMsgtype_File
  | MediaMsgtype_Image
  | MediaMsgtype_Video
  | MediaMsgtype_Audio
  deriving stock (Generic, Eq, Show, Bounded, Enum)

instance J.ToJSON MediaMsgtype where
  toJSON x = J.String $ case x of
    MediaMsgtype_File → Api.msgtypeString Api.MFileType
    MediaMsgtype_Image → Api.msgtypeString Api.MImageType
    MediaMsgtype_Video → Api.msgtypeString Api.MVideoType
    MediaMsgtype_Audio → Api.msgtypeString Api.MAudioType

instance J.FromJSON MediaMsgtype where
  parseJSON jsonValue =
    maybe
      (fail $ "Failed to parse MediaMsgtype from " <> show jsonValue)
      pure
      (find ((jsonValue ==) . J.toJSON) [minBound .. maxBound ∷ MediaMsgtype])

clientEventContentToMediaMsgtype
  ∷ Api.MRoomMessageClientEventContent
  → Maybe MediaMsgtype
clientEventContentToMediaMsgtype = \case
  Api.MRoomMessageClientEventContentMImage _ → Just MediaMsgtype_Image
  Api.MRoomMessageClientEventContentMVideo _ → Just MediaMsgtype_Video
  Api.MRoomMessageClientEventContentMAudio _ → Just MediaMsgtype_Audio
  Api.MRoomMessageClientEventContentMFile _ → Just MediaMsgtype_File
  Api.MRoomMessageClientEventContentMText _ → Nothing
  Api.MRoomMessageClientEventContentOther _ → Nothing


filterByMediaMsgtype
  ∷ ML.MonadLogger m
  ⇒ Maybe MediaMsgtypeFilter
  -- ^ Whitelisted @msgtype@s
  → Api.MRoomMessageClientEventContent
  → Except.ExceptT Text m ()
filterByMediaMsgtype filterList (clientEventContentToMediaMsgtype → mediaMsgType) =
  case (filterList, mediaMsgType) of
    (Nothing, _) →
      L.logDebug $ mconcat
        [ "There is no “msgtype” media filter"
        , " (filter passed)"
        ]
    (Just x, Just y) | y `elem` x →
      L.logDebug $ mconcat
        [ "Media “msgtype” ", (pack . show) y
        , " is one of these: ", (pack . show) x
        , " (filter passed)"
        ]
    (Just _, Nothing) →
      Except.throwE $ mconcat
        [ "“msgtype” is not one of the media types"
        , " (filter not passed)"
        ]
    (Just x, Just y) →
      Except.throwE $ mconcat
        [ "Media “msgtype” ", (pack . show) y
        , " is not one of these: ", (pack . show) x
        , " (filter not passed)"
        ]
