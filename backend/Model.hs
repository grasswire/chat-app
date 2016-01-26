module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Taplike.Schema (MessageUUID, ChannelSlug, unSlug)
import qualified Taplike.Schema as Schema
import TextShow (TextShow)
import TextShow.TH (deriveTextShow)
import TextShow.Data.Time ()
import Taplike.TextShowOrphans ()
import qualified Types as TP
import Types ( RtmEvent(..) )
import Database.Persist.Sql  (fromSqlKey)

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

chanFromEntity :: Entity Channel -> TP.NumberUsersPresent -> [Key User] -> TP.Channel
chanFromEntity entity numPresent members = TP.Channel { TP.channelCreator = TP.UserId (fromSqlKey $ (channelCreator $ entityVal entity))
                                              , TP.channelCreated = channelCreated $ entityVal entity
                                              , TP.channelTopic = TP.ChannelTopic $ channelTopic $ entityVal entity
                                              , TP.channelChannelSlug = TP.ChannelSlug $ unSlug $ channelCrSlug $ entityVal entity
                                              , TP.channelTitle = TP.ChannelTitle $ channelTitle $ entityVal entity
                                              , TP.channelNumUsersPresent = numPresent
                                              , TP.channelColor = TP.ChannelColor ("#" <> channelColor (entityVal entity))
                                              , TP.channelMembers = TP.UserId . fromSqlKey <$> members 
                                              } 
      

rtmMessageFromEntity :: Entity Channel -> Entity Message -> TP.Message 
rtmMessageFromEntity channelEntity msgEntity = 
  let message = entityVal msgEntity
      channel = entityVal channelEntity 
  in TP.Message { TP.messageUser = TP.UserId (fromSqlKey $ messageUser message) 
                , TP.messageText = TP.MessageText (messageText message)
                , TP.messageTS = messageTimestamp message 
                , TP.messageEventTS = Nothing 
                , TP.messageChannel = unSlug (channelCrSlug channel)
                , TP.messageUUID = Schema._messageUuid (messageUuid message)
                , TP.messageMessageLikes = []
              }                                         