{-# LANGUAGE TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Rtm where

import Import

import Taplike.Shared (RtmStartRp(..), Self(..))
import Taplike.ChannelSlug
import qualified Database.Esqueleto as E
import Data.Time.Clock

getRtmStartR :: Handler Value
getRtmStartR = do
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
                  let fiveMinsAgo = addUTCTime (negate 300 :: NominalDiffTime) timeNow
                  runDB (usersPresentQuery (entityKey channel) fiveMinsAgo)
                _ ->  return []
      let jsonResp = case user of
                      Just u -> RtmStartRp url (Just $ Self (entityKey u) (userTwitterScreenName $ entityVal u) (userProfileImageUrl $ entityVal u)) (fmap entityVal users)
                      _      -> RtmStartRp url Nothing (fmap entityVal users)
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
