{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Chat where

import Import hiding (toLower)
import Yesod.WebSockets
import qualified Server as S

import Network.Wai (remoteHost)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Model.Incoming as Incoming
import Taplike.Shared (RtmEvent(..), TS(..), IncomingMessage(..), ChannelCreatedRp(..))
import qualified Taplike.Shared as SH
import Database.Persist.Sql (fromSqlKey)
import Taplike.ChannelSlug
import Data.Char (toLower)
import Data.Text.ICU.Replace

getHealthCheckR :: Handler Text
getHealthCheckR = return "all good"

chatApp :: Text -> Maybe User -> WebSocketsT Handler ()
chatApp channelName user = do
    sendTextData ("Welcome to #" <> channelName)
    app <- getYesod
    (outChan, channel) <- atomically $ do
                chan <- S.lookupOrCreateChannel (chatServer app) (fromStrict channelName)
                return (S.channelBroadcastChan chan, chan)
    inChan <- atomically (dupTChan outChan)
    case user of
      Just u -> do
        _ <- liftIO $ atomically $ S.chanAddClient S.JoinReasonConnected channel (userTwitterUserId u)
        race_
          (ingest inChan)
          (sourceWS $$ mapM_C (atomically . writeTChan outChan . processMessage u))
      Nothing -> ingest inChan
    where ingest chan = forever $ atomically (readTChan chan) >>= sendTextData

processMessage :: User -> RtmEvent -> RtmEvent
processMessage user event = case event of
                              (RtmSendMessage incoming) -> RtmMessage (SH.Message (userTwitterUserId user) ( incomingMessageMessageText incoming) (TS "0") (Just $ TS "0") (SH.incomingMessageChannelId incoming))
                              _ -> RtmHello

getUsername :: YesodRequest -> Maybe TL.Text
getUsername req = Just $ TL.pack $ (show . remoteHost . reqWaiRequest) req

newtype RoomId = RoomId Integer

getChatR :: ChannelSlug -> Handler Html
getChatR slug = do
    channel <- runDB (getBy $ UniqueChannelSlug slug)
    authId <- maybeAuthId
    chatUser <- maybe (return Nothing) (runDB . get) authId
    case channel of
      Just c -> do
        let room = entityVal c
        webSockets $ chatApp (channelTitle room) chatUser
        defaultLayout $(widgetFile "chat-room")
      Nothing -> getHomeR

postNewChatR :: Handler ()
postNewChatR = do
    channel <- requireJsonBody :: Handler Incoming.Channel
    authId  <- maybeAuthId
    case authId of
      Just i -> do
        currentTime <- liftIO getCurrentTime
        let slug = slugify $ Incoming.channelTitle channel
            newChannel  = Channel i (Incoming.channelTitle channel) (Incoming.channelTopic channel) slug currentTime i False
        runDB (insert newChannel) >>= \key -> sendResponseStatus status201 (toJSON (ChannelCreatedRp newChannel (fromSqlKey key) slug))
      Nothing  -> sendResponseStatus status401 ("UNAUTHORIZED" :: Text)

slugify :: Text -> ChannelSlug
slugify = ChannelSlug . makeSlug

makeSlug :: Text -> Text
makeSlug =  replaceAll "[ _]" "-"
          . replaceAll "[^a-z0-9_ ]+" ""
          . T.map toLower

getHomeR :: Handler Html
getHomeR = do
    channels <- runDB (selectList [] [LimitTo 5]) :: Handler [Entity Channel]
    authId <- maybeAuthId
    defaultLayout $ do
        setTitle "Taplike / Home"
        $(widgetFile "homepage")

getLogOutR :: Handler Html
getLogOutR = clearSession >> getHomeR
