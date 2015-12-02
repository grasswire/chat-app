{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson (object, (.=), ToJSON(toJSON))
import Data.Text as T

data NewChannel = NewChannel
  { newChannelTitle :: T.Text
  , newChannelTopic :: T.Text
  } deriving (Show)


instance ToJSON NewChannel where
  toJSON (NewChannel title topic) = object [ "title" .= title
                                           , "topic" .= topic]
