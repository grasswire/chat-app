{-# LANGUAGE TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Rtm where

import Import
import Yesod.WebSockets
import Server

import Taplike.Shared (RtmStartRp, RtmStartRp(..), Self(..))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Data.Aeson (Value)
import qualified Data.Text.Read as TR
import Text.Julius (rawJS)

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
          let maybeInt = TR.decimal roomId
          case maybeInt of
            Right rId -> do
              renderer <- getUrlRender
              let sqlKey = toSqlKey ((fromIntegral . fst $ rId ) :: Int64)
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
                              _ -> RtmStartRp url Nothing (fmap entityVal users)
              returnJson $ jsonResp
            Left err -> sendResponseStatus badRequest400 (pack err :: Text)
        Nothing -> sendResponseStatus badRequest400 ("BADREQUEST: MISSING room_id param" :: Text)
