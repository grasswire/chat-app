{-# LANGUAGE OverloadedStrings #-}

module Types where

import ClassyPrelude
import qualified Data.Text.Lazy as TL

type ClientId = Int64
type ChannelName = TL.Text

data Message
    = Notice TL.Text
    | Tell ClientId TL.Text
    | Broadcast ChannelName ClientId TL.Text
    | Command TL.Text
