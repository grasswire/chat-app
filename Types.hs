{-# LANGUAGE OverloadedStrings #-}

module Types where

import ClassyPrelude

import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as TL
import Network.WebSockets hiding (Message)
import Data.String (IsString, fromString)
import Data.Semigroup
import Data.Aeson (encode, decode)
import Taplike.Shared (RtmEvent(..), RtmEvent)


type ClientId = Int64
type ChannelName = TL.Text

data Message
    = Notice TL.Text
    | Tell ClientId TL.Text
    | Broadcast ChannelName ClientId TL.Text
    | Command TL.Text

instance WebSocketsData RtmEvent where
  fromLazyByteString = fromJust . decode
  toLazyByteString   = encode
