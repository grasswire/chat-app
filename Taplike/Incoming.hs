{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Taplike.Incoming where

import ClassyPrelude
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BL

import Taplike.Shared
import Extra.JSON

data RtmSendMessage = RtmSendMessage
  { _sendMessageSeqnum :: Word64
  , _sendMessageChat   :: ID Chat
  , _sendMessageText   :: Text }

instance ToJSON RtmSendMessage where
  toJSON (RtmSendMessage seqnum chat message) = object
    [ "type"    .= ("message" :: Text)
    , "id"      .= seqnum
    , "channel" .= chat
    , "text"    .= message
    ]

instance FromJSON RtmSendMessage where
  parseJSON (Object v) = RtmSendMessage <$>
                         v .: "id" <*>
                         v .: "channel" <*>
                         v .: "text"
  parseJSON _          = empty
