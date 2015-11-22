{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

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
  case authId of
    Just i -> do
      user <-  runDB $ getBy (UniqueUser $ unUserKey i)
      case user of
        Just u -> do
          mparam <- lookupGetParam "room_id"
          case mparam of
            Just roomId -> do
              let maybeInt = TR.decimal roomId
              case maybeInt of
                Right rId -> do
                  renderer <- getUrlRender
                  let url = renderer $ ChatR (toSqlKey ((fromIntegral . fst $ rId ) :: Int64))
                  returnJson $ RtmStartRp url (Self (unUserKey i) (userTwitterScreenName $ entityVal u) (userProfileImageUrl $ entityVal u)) []
                Left err -> sendResponseStatus badRequest400 (pack err :: Text)
            Nothing -> sendResponseStatus badRequest400 ("BADREQUEST: MISSING room_id param" :: Text)
        Nothing -> failAuth
    Nothing -> failAuth
    where failAuth = sendResponseStatus status401 ("UNAUTHORIZED" :: Text)
