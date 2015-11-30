{-# LANGUAGE OverloadedStrings #-}


module Taplike.MessageSubtype where

import ClassyPrelude
import Database.Persist.Sql

data MessageSubtype
  = MeMS
  | ChangedMS
  | DeletedMS
  | ChannelTopicMS
  | ChannelArchiveMS
  | ChannelUnarchiveMS deriving (Eq, Show)

instance PersistField MessageSubtype where
    toPersistValue subtype = case subtype of
                               MeMS               -> toPersistValue ("me_message" :: Text)
                               ChangedMS          -> toPersistValue ("message_changed" :: Text)
                               DeletedMS          -> toPersistValue ("message_deleted" :: Text)
                               ChannelTopicMS     -> toPersistValue ("channel_topic" :: Text)
                               ChannelArchiveMS   -> toPersistValue ("channel_archive" :: Text)
                               ChannelUnarchiveMS -> toPersistValue ("channel_unarchive" :: Text)
    fromPersistValue (PersistText txt) = case txt of
                                            "me_message"        -> Right MeMS
                                            "message_changed"   -> Right ChangedMS
                                            "message_deleted"   -> Right DeletedMS
                                            "channel_topic"     -> Right ChannelTopicMS
                                            "channel_archive"   -> Right ChannelArchiveMS
                                            "channel_unarchive" -> Right ChannelUnarchiveMS
                                            unrecognized        -> Left $ "unrecognized message subtype:" ++ unrecognized
    fromPersistValue x = Left $ "Not a PersistText " ++ pack (show x)

instance PersistFieldSql MessageSubtype where
    sqlType _ =  SqlString
