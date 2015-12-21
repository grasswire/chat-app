{-# LANGUAGE TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Rtm where

import Import hiding ((==.), (>=.))


import           Data.Time.Clock
import           DataStore
import           Taplike.Schema
import qualified Database.Esqueleto   as E
import           Database.Esqueleto   ((==.), (^.), (>=.), (&&.), val, (?.), LeftOuterJoin)
import qualified Types                as TP
import           Control.Concurrent   (forkIO)
import           Taplike.Shared       (userFromEntity)
import           Database.Persist.Sql (fromSqlKey)
import           Types                (RtmStartRp(..), Self(..))

-- data RtmStartRp = RtmStartRp
--   { rtmStartUrl      :: Text
--   , rtmStartSelf     :: Maybe Self
--   , rtmStartUsers    :: [User]
--   , rtmStartChannels :: [Channel]
--   }

getRtmStartR :: Handler Value
getRtmStartR = do 
  app <- getYesod
  authId <- maybeAuthId
  user <- case authId of
            Just i -> fmap (Entity i) <$> runDB (get i)
            _      -> return Nothing
  case user of 
    Nothing -> anonymousRtm 
    Just u  -> authenticatedRtm u
  where anonymousRtm       = sendResponseStatus badRequest400 ("BADREQUEST: MISSING channel_slug param" :: Text)
        authenticatedRtm u = do 
          users <- runDB (usersInUserChannels (entityKey u))
          let response = RtmStartRp (Just $ Self (TP.UserId $ fromSqlKey $ entityKey u) 
                                    (userTwitterScreenName $ entityVal u) (userProfileImageUrl $ entityVal u)) 
                                    (fmap userFromEntity users) []
          returnJson response
          
usersPresentQuery ::  MonadIO m => Key Channel -> UTCTime -> SqlPersistT m [Entity User]
usersPresentQuery chanKey lastseen = E.select $
                                     E.from $ \user -> do
                                     E.where_ $ E.exists $
                                                E.from $ \heartbeat ->
                                                E.where_ (heartbeat ^. HeartbeatChannel ==. val chanKey &&.
                                                  heartbeat ^. HeartbeatUser ==. user ^. UserId &&.
                                                  heartbeat ^. HeartbeatLastSeen >=. val lastseen)
                                     return user

channelMembersQuery ::  MonadIO m => Key Channel -> SqlPersistT m [Entity User]
channelMembersQuery chanKey = E.select $
                              E.from $ \user -> do
                              E.where_ $ E.exists $
                                         E.from $ \membership ->
                                         E.where_ (membership ^. MembershipChannel ==. val chanKey &&. 
                                         membership ^. MembershipUser ==. user ^. UserId)
                              return user

-- select * from channel left join membership on channel.id = membership.channel where membership.in_channel = true;
usersChannels :: MonadIO m => Key User -> SqlPersistT m [(Entity Channel, E.Value (Maybe (Key User)))]
usersChannels userKey = do 
  myChannels <- userMemberships userKey
  E.select $
    E.from $ \(channel `E.LeftOuterJoin` membership) -> do 
    E.on (E.just (channel ^. ChannelId) ==. membership ?. MembershipChannel) 
    E.where_ (channel ^. ChannelId `E.in_` E.valList (E.unValue <$> myChannels) &&. membership ?. MembershipInChannel ==. E.just (E.val True))
    return (channel, membership ?. MembershipUser)
  



-- list of users who were/are members of any channel I'm a member of 
-- this is analogous to Slack's rtm.start which includes a list of all members of a team 
-- the query should be ~ to: select * from taplike_users where id in (select "user" from membership where channel in (select channel from membership where "user" = 3));
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

userMemberships ::  MonadIO m => Key User -> SqlPersistT m [E.Value (Key Channel)]                             
userMemberships userKey = E.select $
                        E.from $ \membership -> do
                        E.where_ (membership ^. MembershipUser ==. val userKey)
                        return   (membership ^. MembershipChannel)