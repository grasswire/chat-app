{-# LANGUAGE TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Queries where

import           Import hiding ((==.), (>=.))
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((==.), (^.), (&&.), val, (?.))
import Database.Persist.Sql (rawSql)
import Database.Persist.Sql (fromSqlKey)


          
-- list of all channels along with their current members for the given user 
usersChannelsWithMembers :: MonadIO m => Key User -> SqlPersistT m [(Entity Channel, E.Value (Maybe (Key User)))]
usersChannelsWithMembers userKey = do 
  myChannels <- userMemberships userKey
  E.select $
    E.from $ \(channel `E.LeftOuterJoin` membership) -> do 
    E.on (E.just (channel ^. ChannelId) ==. membership ?. MembershipChannel) 
    E.where_ (channel ^. ChannelId `E.in_` E.valList (E.unValue <$> myChannels) &&. membership ?. MembershipInChannel ==. E.just (E.val True))
    return (channel, membership ?. MembershipUser)

usersChannels :: MonadIO m => Key User -> SqlPersistT m [Entity Channel]
usersChannels userKey = do 
  myChannels <- userMemberships userKey
  E.select $
    E.from $ \channel -> do 
    E.where_ (channel ^. ChannelId `E.in_` E.valList (E.unValue <$> myChannels))
    return channel
  
-- list of users who are or were members of any channel I'm a member of 
usersInUserChannels ::  MonadIO m => Key User -> SqlPersistT m [Entity User]
usersInUserChannels userKey = do 
  myChannels <- userMemberships userKey
  users      <- usersInMyChans myChannels
  E.select $ 
    E.from $ \user -> do
    E.where_ (user ^. UserId `E.in_` E.valList (E.unValue <$> users))
    return user
  where usersInMyChans chans = E.select $ 
                               E.from $ \membership -> do
                               E.where_ (membership ^. MembershipChannel `E.in_` E.valList (E.unValue <$> chans))
                               return   (membership ^. MembershipUser)

-- list of current members for a given channel
membersByChannel :: MonadIO m => Key Channel -> SqlPersistT m [Key User]
membersByChannel chanKey = do
  users <- E.select $
           E.from $ \membership -> do 
           E.where_ (membership ^. MembershipChannel ==. val chanKey &&. membership ^. MembershipInChannel ==. E.val True)
           return (membership ^. MembershipUser)
  return (E.unValue <$> users)
  
-- list of users who are or were members for a given channel
usersByChannel ::  MonadIO m => Key Channel -> SqlPersistT m [Entity User]
usersByChannel channel = do 
  users      <- usersInChan
  E.select $ 
    E.from $ \user -> do
    E.where_ (user ^. UserId `E.in_` E.valList (E.unValue <$> users))
    return user
  where usersInChan  = E.select $ 
                       E.from $ \membership -> do
                       E.where_ (membership ^. MembershipChannel ==. val channel)
                       return   (membership ^. MembershipUser)

userMemberships ::  MonadIO m => Key User -> SqlPersistT m [E.Value (Key Channel)]                             
userMemberships userKey = E.select $
                        E.from $ \membership -> do
                        E.where_ (membership ^. MembershipUser ==. val userKey)
                        return   (membership ^. MembershipChannel)
                        
messageHistorySql ::  Text
messageHistorySql  = "select ?? from (select ROW_NUMBER() over (partition by channel order by timestamp desc)" <> 
                              -- "as r, t.* from message t) message join shouldbechannel c on (c.id = message.channel) where message.r <= ? and message.channel in (" <>  
                              "as r, t.* from message t) message join shouldbechannel c on (c.id = message.channel) where message.r <= ? and message.channel in ?" -- <>  
                              -- intercalate "," (pack . show . fromSqlKey <$> channels) <> ");"
                              
                              
messageHistory :: MonadIO m => Int -> [Key Channel] -> ReaderT SqlBackend m [(Entity Message, Entity Channel)]
messageHistory limit channels = return [] -- rawSql (messageHistorySql) [PersistInt64 $ fromIntegral limit, PersistList (PersistInt64 . fromSqlKey <$> channels)]