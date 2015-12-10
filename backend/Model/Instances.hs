{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings    #-}

module Model.Instances where

import ClassyPrelude
import Types
import Control.Applicative (empty)
import Data.Aeson (object, (.=), (.:), ToJSON(toJSON), FromJSON(parseJSON))
import Data.Aeson.Types (Value(..))

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

deriving instance FromJSON ChannelTopic
deriving instance FromJSON ChannelTitle
deriving instance FromJSON ChannelColor
deriving instance ToJSON ChannelTitle
deriving instance ToJSON ChannelTopic
deriving instance ToJSON ChannelColor
