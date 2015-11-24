module Taplike.Slug where

import ClassyPrelude
import Yesod.Persist.Core (YesodDB)
import Database.Persist.Types (Entity)

class Slug slug where
    type SlugEntity slug
    lookupSlug :: slug -> YesodDB site (Maybe (Entity (SlugEntity slug)))
