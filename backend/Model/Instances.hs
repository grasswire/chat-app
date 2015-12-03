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
                         v .: "topic"
  parseJSON _          = empty

instance ToJSON NewChannel where
  toJSON (NewChannel title topic) = object [ "title" .= title
                                           , "topic" .= topic]

deriving instance FromJSON NewChannelTopic
deriving instance FromJSON NewChannelTitle
deriving instance ToJSON NewChannelTitle
deriving instance ToJSON NewChannelTopic
