{-# LANGUAGE TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Rtm where

import Import
import Server

import Database.Persist.Sql (toSqlKey)
import Taplike.Shared (RtmStartRp(..), Self(..))
import Text.Read (readEither)
import Taplike.ChatRoomSlug


getRtmStartR :: Handler Value
getRtmStartR = do
  authId <- maybeAuthId
  app <- getYesod
  user <- case authId of
            Just i -> runDB $ getBy (UniqueUser $ unUserKey i)
            _      -> return Nothing
  mparam <- lookupGetParam "room_id"
  case mparam of
    Just roomId -> do
      let chatSlug = ChatRoomSlug roomId
      renderer <- getUrlRender
      let url = renderer $ ChatR chatSlug
      chatRoom <- runDB (getBy $ UniqueChatRoomSlug chatSlug)
      users <- case chatRoom of
                Just room -> do
                  userIds <- do
                    channel <- liftIO $ atomically $ lookupChannel (chatServer app) (fromStrict $ chatRoomTitle $ entityVal room)
                    case channel of
                      Just ch -> liftIO $ atomically (listUsers ch)
                      _ -> return []
                  runDB $ selectList [UserTwitterUserId <-. userIds] []
                _ -> return []
      let jsonResp = case user of
                      Just u -> RtmStartRp url (Just $ Self (unUserKey $ entityKey u) (userTwitterScreenName $ entityVal u) (userProfileImageUrl $ entityVal u)) (fmap entityVal users)
                      _      -> RtmStartRp url Nothing (fmap entityVal users)
      returnJson jsonResp
    Nothing -> sendResponseStatus badRequest400 ("BADREQUEST: MISSING room_id param" :: Text)
