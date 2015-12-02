module Model
       ( module Model
       , module Types
       ) where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Taplike.ChannelSlug (ChannelSlug)
import TextShow (TextShow)
import TextShow.TH (deriveTextShow)
import TextShow.Data.Time ()
import Taplike.TextShowOrphans ()
import Types

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

deriving instance TextShow (Key User)
deriving instance TextShow (Key Channel)

deriveTextShow ''User
deriveTextShow ''Channel
