module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Taplike.ChannelSlug (ChannelSlug, unSlug)
import TextShow (TextShow)
import TextShow.TH (deriveTextShow)
import TextShow.Data.Time ()
import Taplike.TextShowOrphans ()
import qualified Types as TP
import Database.Persist.Sql  (fromSqlKey)
-- import Data.Monoid ((<>))


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

chanFromEntity :: Entity Channel -> TP.NumberUsersPresent -> TP.Channel
chanFromEntity entity numPresent = TP.Channel { TP.channelCreator = TP.UserId (fromSqlKey $ entityKey entity)
                                              , TP.channelCreated = channelCreated $ entityVal entity
                                              , TP.channelTopic = TP.ChannelTopic $ channelTopic $ entityVal entity
                                              , TP.channelSlug = TP.ChannelSlug $ unSlug $ channelCrSlug $ entityVal entity
                                              , TP.channelTitle = TP.ChannelTitle $ channelTitle $ entityVal entity
                                              , TP.channelNumUsersPresent = numPresent
                                              , TP.channelColor = TP.ChannelColor $ ("#" <> (channelColor $ entityVal entity))
                                              }
