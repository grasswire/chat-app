{-# LANGUAGE TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Rtm where

import Import hiding ((==.), (>=.))


import           Data.Time.Clock
import           DataStore
import           Taplike.Schema
import qualified Database.Esqueleto   as E
import           Database.Esqueleto   ((==.), (^.), (>=.), (&&.), val)
import qualified Types                as TP
import           Control.Concurrent   (forkIO)
import           Taplike.Shared       (userFromEntity)
import           Database.Persist.Sql (fromSqlKey)
import           Types                (RtmStartRp(..), Self(..))

getRtmStartR :: Handler Value
getRtmStartR = do
  app <- getYesod
  authId <- maybeAuthId
  user <- case authId of
            Just i -> fmap (Entity i) <$> runDB (get i)
            _      -> return Nothing
  mparam <- lookupGetParam "channel_slug"
  case mparam of
    Just slug -> do
      renderer <- getUrlRender
      let channelSlug = ChannelSlug slug
          url = renderer $ ChatR channelSlug
      maybeChan <- runDB (getBy $ UniqueChannelSlug channelSlug)
      now <- liftIO getCurrentTime
      users <- case maybeChan of
                Just channel -> do
                  maybe (return ()) (\key -> void $ runDB (upsert (Heartbeat key now (entityKey channel)) 
                                 [HeartbeatLastSeen =. now])) authId
                  let timeAgo = addUTCTime (negate 120 :: NominalDiffTime) now
                  runDB (usersPresentQuery (entityKey channel) timeAgo)
                _ ->  return []
      members <- case maybeChan of 
        Just channel -> runDB (channelMembersQuery $ entityKey channel)          
        _            -> return []
      case maybeChan of
        Just chan -> liftIO $ void $ forkIO (void $ runRedisAction (redisConn app) 
                                            (setChannelPresence (fromIntegral $ length users :: Integer) 
                                            (channelCrSlug $ entityVal chan)))
        _         -> return ()
      let jsonResp = case user of
                      Just u -> RtmStartRp url (Just $ Self (TP.UserId $ fromSqlKey $ entityKey u) 
                                               (userTwitterScreenName $ entityVal u) (userProfileImageUrl $ entityVal u)) 
                                               (fmap userFromEntity users) (fmap userFromEntity members)
                      _      -> RtmStartRp url Nothing (fmap userFromEntity users) (fmap userFromEntity members)
      returnJson jsonResp
    Nothing -> sendResponseStatus badRequest400 ("BADREQUEST: MISSING channel_slug param" :: Text)

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

