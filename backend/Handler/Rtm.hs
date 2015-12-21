{-# LANGUAGE TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Rtm where

import Import hiding ((==.), (>=.))

import           Data.Time.Clock
import           DataStore
import           Taplike.Schema
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((==.), (^.), (>=.), (&&.), val, (?.))
import qualified Types as TP
import           Control.Concurrent (forkIO)
import           Taplike.Shared (userFromEntity)
import           Database.Persist.Sql (fromSqlKey)
import           Types (RtmStartRp(..), Self(..))

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
  where anonymousRtm = do 
          mParam <- lookupGetParam "channel_slug" 
          case mParam of 
            Nothing -> sendResponseStatus badRequest400 ("BADREQUEST: anonymous user must provide a channel_slug param" :: Text)
            Just slug -> do 
              maybeChan <- runDB (getBy $ UniqueChannelSlug (ChannelSlug slug))
              case maybeChan of 
                Nothing -> sendResponseStatus status404 ("NOTFOUND: no channel found with the provided slug" :: Text)
                Just channel -> do 
                  members <- runDB (membersByChannel (entityKey channel))
                  users   <- runDB (usersByChannel (entityKey channel))
                  let response = RtmStartRp Nothing (fmap userFromEntity users) [chanFromEntity channel (TP.NumberUsersPresent 0) members]
                  returnJson response  
        authenticatedRtm u = do 
          users      <- runDB (usersInUserChannels (entityKey u))
          myChannels <- runDB (usersChannels (entityKey u))
          let results = second E.unValue <$> myChannels
              grouped = (map (\l@((h,_):_) -> (h, mapMaybe snd l)) . groupBy ((==) `on` fst)) results
              response = RtmStartRp (Just $ Self (TP.UserId $ fromSqlKey $ entityKey u) 
                                    (userTwitterScreenName $ entityVal u) (userProfileImageUrl $ entityVal u)) 
                                    (fmap userFromEntity users) (uncurry (flip chanFromEntity $ TP.NumberUsersPresent 0) <$> grouped)
          returnJson response
          
-- list of all channels along with their current members for the given user 
usersChannels :: MonadIO m => Key User -> SqlPersistT m [(Entity Channel, E.Value (Maybe (Key User)))]
usersChannels userKey = do 
  myChannels <- userMemberships userKey
  E.select $
    E.from $ \(channel `E.LeftOuterJoin` membership) -> do 
    E.on (E.just (channel ^. ChannelId) ==. membership ?. MembershipChannel) 
    E.where_ (channel ^. ChannelId `E.in_` E.valList (E.unValue <$> myChannels) &&. membership ?. MembershipInChannel ==. E.just (E.val True))
    return (channel, membership ?. MembershipUser)
  
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