{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings    #-}

module Model.Instances where

import           ClassyPrelude
import           Types
import           Data.Aeson
import           Control.Applicative (empty)

instance FromJSON NewChannel where
  parseJSON (Object v) = NewChannel <$>
                         v .: "title" <*>
                         v .: "topic"
  parseJSON _          = empty
