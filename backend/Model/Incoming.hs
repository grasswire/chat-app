{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Model.Incoming where

import Control.Applicative (empty)
import Data.Aeson
import ClassyPrelude
import qualified Data.Text as T

data Channel = Channel
  { channelTitle :: T.Text
  , channelTopic :: T.Text
  } deriving (Show)


instance ToJSON Channel where
  toJSON (Channel title topic) = object [ "title" .= title
                                        , "topic" .= topic]

instance FromJSON Channel where
  parseJSON (Object v) = Channel <$>
                         v .: "title" <*>
                         v .: "topic"
  parseJSON _          = empty
