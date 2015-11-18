{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Model.Incoming where

import Control.Applicative (empty)
import Data.Aeson
import Data.Monoid
import ClassyPrelude
-- import Data.Aeson (toJSON, Value(..))
import qualified Data.Text as T
import Data.Aeson

data ChatRoom = ChatRoom
  { title       :: T.Text
  , description :: T.Text
  } deriving (Show)


instance ToJSON ChatRoom where
  toJSON (ChatRoom t d) = object [ "title" .= t,
                                  "description" .= d]

instance FromJSON ChatRoom where
  parseJSON (Object v) = ChatRoom <$>
                         v .: "title" <*>
                         v .: "description"
  parseJSON _          = empty
