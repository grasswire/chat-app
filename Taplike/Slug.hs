module Taplike.Slug where

import ClassyPrelude
import Yesod.Persist.Core (YesodDB)
import Database.Persist.Types (Entity)
import Taplike.ChatRoomSlug (ChatRoomSlug)

class Slug slug where
    type SlugEntity slug
    lookupSlug :: slug -> YesodDB site (Maybe (Entity (SlugEntity slug)))

instance Slug ChatRoomSlug where
    type SlugEntity ChatRoomSlug = ChatRoom
    lookupSlug = getBy . UniqueChatRoomSlug
