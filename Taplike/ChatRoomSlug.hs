module Taplike.ChatRoomSlug where

import Yesod.Core.Dispatch (PathPiece)
import ClassyPrelude
import Data.Aeson(Value(..), ToJSON(toJSON), FromJSON(parseJSON))
import Data.Aeson.Types (typeMismatch)
import Taplike.Slug
import Database.Persist.Class (getBy)
import Model (ChatRoom)

newtype ChatRoomSlug = ChatRoomSlug Text
    deriving (PathPiece, Show)

instance ToJSON ChatRoomSlug where
  toJSON (ChatRoomSlug slug) = String slug

instance FromJSON ChatRoomSlug where
  parseJSON (String s) = pure $ ChatRoomSlug s
  parseJSON invalid = typeMismatch "ChatRoomSlug" invalid

instance Slug ChatRoomSlug where
    type SlugEntity ChatRoomSlug = ChatRoom
    lookupSlug = getBy . UniqueChatRoomSlug
