{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings    #-}

module Model.Instances where

import ClassyPrelude
import Types
import Control.Applicative (empty)
import Data.Aeson (object, (.=), (.:), ToJSON(toJSON), FromJSON(parseJSON))
import Data.Aeson.Types (Value(..))
import Data.UUID.Aeson ()

instance FromJSON NewChannel where
  parseJSON (Object v) = NewChannel <$>
                         v .: "title" <*>
                         v .: "topic" <*>
                         v .: "color"
  parseJSON _          = empty

instance ToJSON NewChannel where
  toJSON (NewChannel title topic color) = object [ "title" .= title
                                                 , "topic" .= topic
                                                 , "color" .= color]

instance FromJSON NewMessageLike where
  parseJSON (Object v) = NewMessageLike <$>
                         v .: "message_id" <*>
                         v .: "channel"
  parseJSON _          = empty

instance ToJSON NewMessageLike where
  toJSON (NewMessageLike messageId channel) = object [ "message_id" .= messageId
                                                     , "channel"    .= channel
                                                     ]

deriving instance FromJSON ChannelTopic
deriving instance FromJSON ChannelTitle
deriving instance FromJSON ChannelColor
deriving instance FromJSON MessageId
deriving instance FromJSON UserId 
deriving instance FromJSON ChannelSlug
deriving instance ToJSON ChannelTitle
deriving instance ToJSON ChannelTopic
deriving instance ToJSON ChannelColor
deriving instance ToJSON MessageId
deriving instance ToJSON ChannelSlug
deriving instance ToJSON UserId 

instance ToJSON OkResponse where 
  toJSON _ = object [ "ok" .= True ]
