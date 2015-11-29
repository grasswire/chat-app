module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Taplike.ChatRoomSlug (ChatRoomSlug)
import TextShow (TextShow, showb)
import TextShow.TH (deriveTextShow)
import Database.Persist.Sql (fromSqlKey)
import TextShow.Data.Time ()


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance TextShow (Key User) where
    showb (UserKey key) = showb key

instance TextShow (Key ChatRoom) where
  showb = showb . fromSqlKey

deriveTextShow ''User
deriveTextShow ''ChatRoom
