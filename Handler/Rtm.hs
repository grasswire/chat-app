{-# LANGUAGE TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Rtm where

import Import
import Server

import Database.Persist.Sql (toSqlKey)
import Taplike.Shared (RtmStartRp(..), Self(..))
import Text.Read (readEither)


getRtmStartR :: Handler Value
getRtmStartR = do
  authId <- maybeAuthId
  app <- getYesod
  case authId of
    Just i -> do
      user <-  runDB $ getBy (UniqueUser $ unUserKey i)
      mparam <- lookupGetParam "room_id"
      case mparam of
        Just roomId -> do
          let maybeInt = readEither (unpack roomId) :: Either String Int64
          case maybeInt of
            Right rId -> do
              renderer <- getUrlRender
              let sqlKey = toSqlKey rId
              let url = renderer $ ChatR sqlKey
              chatRoom <- runDB (get sqlKey)
              users <- case chatRoom of
                        Just room -> do
                          userIds <- do
                            channel <- liftIO $ atomically $ lookupChannel (chatServer app) (fromStrict $ chatRoomTitle room)
                            case channel of
                              Just ch -> liftIO $ atomically (listUsers ch)
                              _ -> return []
                          runDB $ selectList [UserTwitterUserId <-. userIds] []
                        _ -> return []
              let jsonResp = case user of
                              Just u -> RtmStartRp url (Just $ Self (unUserKey i) (userTwitterScreenName $ entityVal u) (userProfileImageUrl $ entityVal u)) (fmap entityVal users)
                              _      -> RtmStartRp url Nothing (fmap entityVal users)
              returnJson jsonResp
            Left err -> sendResponseStatus badRequest400 (pack err :: Text)
        Nothing -> sendResponseStatus badRequest400 ("BADREQUEST: MISSING room_id param" :: Text)
