module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Taplike.ChatRoomSlug (ChatRoomSlug)
import TextShow (TextShow)
import TextShow.TH (deriveTextShow)
import TextShow.Data.Time ()
import Taplike.TextShowOrphans ()

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

deriving instance TextShow (Key User)
deriving instance TextShow (Key ChatRoom)

deriveTextShow ''User
deriveTextShow ''ChatRoom
