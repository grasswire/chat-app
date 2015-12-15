{-# LANGUAGE TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Rtm where

import Import hiding ((==.), (>=.))


import           Data.Time.Clock
import           DataStore
import           Taplike.Schema
import qualified Database.Esqueleto   as E
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
      users <- case maybeChan of
                Just channel -> do
                  timeNow <- liftIO getCurrentTime
                  let hourAgo = addUTCTime (negate 3600 :: NominalDiffTime) timeNow
                  runDB (usersPresentQuery (entityKey channel) hourAgo)
                _ ->  return []
      case maybeChan of
        Just chan -> liftIO $ void $ forkIO (void $ runRedisAction (redisConn app) (setChannelPresence (fromIntegral $ length users :: Integer) (channelCrSlug $ entityVal chan)))
        _ -> return ()
      let jsonResp = case user of
                      Just u -> RtmStartRp url (Just $ Self (TP.UserId $ fromSqlKey $ entityKey u) (userTwitterScreenName $ entityVal u) (userProfileImageUrl $ entityVal u)) (fmap userFromEntity users)
                      _      -> RtmStartRp url Nothing (fmap userFromEntity users)
      returnJson jsonResp
    Nothing -> sendResponseStatus badRequest400 ("BADREQUEST: MISSING channel_slug param" :: Text)

usersPresentQuery ::  MonadIO m => Key Channel -> UTCTime -> SqlPersistT m [Entity User]
usersPresentQuery chanKey lastseen = E.select $
                                     E.from $ \user -> do
                                     E.where_ $ E.exists $
                                                E.from $ \heartbeat ->
                                                E.where_ (heartbeat E.^. HeartbeatChannel E.==. E.val chanKey E.&&.
                                                  heartbeat E.^. HeartbeatUser E.==. user E.^. UserId E.&&.
                                                  heartbeat E.^. HeartbeatLastSeen E.>=. E.val lastseen)
                                     return user

