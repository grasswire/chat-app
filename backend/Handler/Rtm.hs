{-# LANGUAGE TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Rtm where

import Import
import qualified Server as S

import Taplike.Shared (RtmStartRp(..), Self(..))
import Taplike.ChannelSlug
import Database.Persist.Sql (toSqlKey, fromSqlKey)


getRtmStartR :: Handler Value
getRtmStartR = do
  authId <- maybeAuthId
  app <- getYesod
  user <- case authId of
            Just i -> fmap (Entity i) <$> runDB (get i)
            _      -> return Nothing
  let key = entityKey <$> user
  mparam <- lookupGetParam "room_id"
  case mparam of
    Just roomId -> do
      let chatSlug = ChannelSlug roomId
      renderer <- getUrlRender
      let url = renderer $ ChatR chatSlug
      channel <- runDB (getBy $ UniqueChannelSlug chatSlug)
      users <- case channel of
                Just room -> do
                  userIds <- do
                    serverChannel <- liftIO $ atomically $ S.lookupChannel (chatServer app) (entityKey room)
                    case serverChannel of
                      Just ch -> liftIO $ atomically (S.listUsers ch)
                      _ -> return []
                  -- runDB $ selectList [UserTwitterUserId <-. userIds] []
                  return []
                _ -> return []
      let jsonResp = case user of
                      Just u -> RtmStartRp url (Just $ Self (entityKey u) (userTwitterScreenName $ entityVal u) (userProfileImageUrl $ entityVal u)) (fmap entityVal users)
                      _      -> RtmStartRp url Nothing (fmap entityVal users)
      returnJson jsonResp
    Nothing -> sendResponseStatus badRequest400 ("BADREQUEST: MISSING room_id param" :: Text)
