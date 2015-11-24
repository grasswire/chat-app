module Taplike.ChatRoomSlug where

import Yesod.Core.Dispatch (PathPiece)
import ClassyPrelude
import Data.Aeson(Value(..), ToJSON(toJSON), FromJSON(parseJSON))
import Data.Aeson.Types (typeMismatch)
import Database.Persist.Sql

newtype ChatRoomSlug = ChatRoomSlug Text
    deriving (PathPiece, Show)

instance ToJSON ChatRoomSlug where
  toJSON (ChatRoomSlug slug) = String slug

instance FromJSON ChatRoomSlug where
  parseJSON (String s) = pure $ ChatRoomSlug s
  parseJSON invalid = typeMismatch "ChatRoomSlug" invalid

instance PersistField ChatRoomSlug where
    toPersistValue (ChatRoomSlug slug) = toPersistValue slug
    fromPersistValue (PersistText txt) = Right $ ChatRoomSlug txt
    fromPersistValue x = Left $ "Not a PersistText " ++ pack (show x)

instance PersistFieldSql ChatRoomSlug where
    sqlType _ =  SqlString
