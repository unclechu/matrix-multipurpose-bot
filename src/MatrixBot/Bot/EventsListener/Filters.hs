{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Events filtering implementation
module MatrixBot.Bot.EventsListener.Filters
  ( UsersFilter
  , RoomsFilter
  , filterByUser
  , filterByRoom
  ) where

import qualified Control.Monad.Trans.Except as Except
import Data.Text (Text, pack)
import qualified MatrixBot.SharedTypes as T
import qualified Data.List.NonEmpty as NE
import qualified MatrixBot.Log as L
import qualified Control.Monad.Logger as ML


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
      L.logDebug "There is no user filter, filter passed…"
    Just x | userId `elem` x →
      L.logDebug $ mconcat
        [ "User ", T.printMxid userId
        , " is one of these (filter passed): ", pack . show $ x
        ]
    Just x →
      Except.throwE $ mconcat
        [ "User ", T.printMxid userId
        , " is not one of these: ", pack . show $ x
        ]


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
      L.logDebug "There is no room filter, filter passed…"
    Just x | roomId `elem` x →
      L.logDebug $ mconcat
        [ "Room ", T.printRoomId roomId
        , " is one of these (filter passed): ", pack . show $ x
        ]
    Just x →
      Except.throwE $ mconcat
        [ "Room ", T.printRoomId roomId
        , " is not one of these: ", pack . show $ x
        ]


type UsersFilter = NE.NonEmpty T.Mxid
type RoomsFilter = NE.NonEmpty T.RoomId
