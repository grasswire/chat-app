{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Taplike.Incoming where

import ClassyPrelude
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BL

import Taplike.Shared
import Extra.JSON
