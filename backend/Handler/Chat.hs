{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Chat where
import           Text.Julius
import           Import hiding (toLower)
import           Yesod.WebSockets
import qualified Server as S
import           Network.Wai (remoteHost)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import           Types (RtmEvent(..), IncomingMessage(..), ChannelCreatedRp(..), ReplyOk(..))
import           Taplike.Shared (userFromEntity)
import           Database.Persist.Sql (fromSqlKey)
import           Taplike.Schema
import           Data.Char (toLower)
import           Data.Text.ICU.Replace
import qualified Database.Redis as Redis
import qualified Types as TP
import Model.Instances ()
import DataStore
import Control.Concurrent (forkIO)
import qualified Control.Exception.Lifted as EL
import Network.WebSockets (ConnectionException)
import Handler.Home (getHomeR)


getHealthCheckR :: Handler Text
getHealthCheckR = return "all good!!"

type WSExceptionHandler = ConnectionException -> WebSocketsT Handler ()

chatApp :: WSExceptionHandler -> Key Channel -> ChannelSlug 
            -> Maybe (Entity User) -> WebSocketsT Handler ()
chatApp exceptionHandler channelId channelSlug userEntity = 
  flip EL.catch exceptionHandler $ do
    sendTextData RtmHello
    app <- getYesod
    chan   <- liftIO $ S.lookupOrCreateChannel (redisConn app) (chatServer app)
                channelSlug
    toSend <- atomically $ dupTChan (S.channelBroadcastChan chan)
    case userEntity of
      Just u  -> race_ (outbound toSend) (inbound app u chan)
      Nothing ->        outbound toSend
  where
    outbound chan = forever $ atomically (readTChan chan) >>= sendTextData
    inbound (App { redisConn }) user chan = do
      let wasSeen = do now <- liftIO getCurrentTime
                       void $ runDB 
                        (upsert 
                            (Heartbeat (entityKey user) now channelId) 
                            [HeartbeatLastSeen =. now])

      void $ liftIO $ do
        atomically $ S.chanAddClient S.JoinReasonConnected chan
                    (userTwitterUserId $ entityVal user)
        void . runRedisAction redisConn $ 
                 S.broadcastEvent channelSlug 
                 (TP.RtmPresenceChange (TP.PresenceChange (userFromEntity user) TP.PresenceActive))
        void . runRedisAction redisConn $ incrChannelPresence channelSlug

      lift wasSeen

      sourceWS $$ mapM_C $ \ case
        RtmHeartbeat _ -> lift wasSeen
        RtmPing ping -> sendTextData $ RtmPong (TP.Pong $ TP.pingId ping)
        inEvent -> do
          ackMessage inEvent
          runInnerHandler <- lift handlerToIO
          void $ liftIO $ forkIO $ runInnerHandler $ do
            wasSeen
            ts <- liftIO getCurrentTime
            let processedMsg = processMessage (entityKey user) inEvent ts
            maybe (return ()) (\e -> void 
                  (liftIO $ runRedisAction redisConn $ 
                          S.broadcastEvent channelSlug e) >> 
                          persistEvent (entityKey user) e) processedMsg
          pure ()

    persistEvent :: UserId -> RtmEvent -> Handler ()
    persistEvent userId event = case event of
                                  RtmMessage msg -> 
                                          void $ runDB $ insert 
                                            (Message userId 
                                              (TP.unMessageText $ TP.messageText msg) 
                                              (TP.messageTS msg) channelId 
                                              (MessageUUID $ TP.messageUUID msg))
                                  _              -> return ()

    ackMessage (RtmSendMessage incoming) = do
      now <- liftIO getCurrentTime
      sendTextData (RtmReplyOk 
                     (ReplyOk 
                       (TP.incomingMessageUUID incoming) 
                       (Just now) 
                       (Just $ TP.unMessageText $ 
                               incomingMessageMessageText incoming)))
    ackMessage _ = return ()

wsExceptionHandler :: Redis.Connection -> ChannelSlug -> ConnectionException -> WebSocketsT Handler ()
wsExceptionHandler conn channelSlug e = do
  liftIO $ putStrLn "decrementing chan presence"
  _ <- liftIO . void . runRedisAction conn $ decrChannelPresence channelSlug
  throwM e

processMessage :: UserId -> RtmEvent -> UTCTime -> Maybe RtmEvent
processMessage userId event eventTS = case event of
                              (RtmSendMessage incoming) -> 
                                      Just $ RtmMessage 
                                        (TP.Message (TP.UserId $ fromSqlKey userId )
                                          (incomingMessageMessageText incoming) 
                                          (TP.incomingMessageTS incoming) 
                                          (Just eventTS) 
                                          (TP.incomingMessageChannelId incoming) 
                                          (TP.incomingMessageUUID incoming))
                              _                         -> Nothing

getUsername :: YesodRequest -> Maybe TL.Text
getUsername req = Just $ TL.pack $ (show . remoteHost . reqWaiRequest) req

getChatR :: ChannelSlug -> Handler Html
getChatR slug = do
    app <- getYesod
    channel <- runDB (getBy $ UniqueChannelSlug slug)
    authId <- maybeAuthId
    renderFuncP <- getUrlRenderParams
    renderFunc <- getUrlRender
    let rtmStartUrl = renderFuncP RtmStartR [("channel_slug", unSlug slug)]
        signature = "chatroom" :: Text
        modalSignin = $(widgetFile "partials/modals/signin")
        redirectUrl = renderFunc $ (ChatR slug)
        loginWithChatRedirect = renderFuncP TwitterAuthR [("redirect_url", redirectUrl)]
    chatUser <- maybe (return Nothing) 
                  (\userId -> fmap (Entity userId) <$> 
                          runDB (get userId)) authId
    case channel of
      Just c -> do
        let room = entityVal c
            isLoggedIn = isJust authId
            chanSlug = channelCrSlug room
        webSockets $ chatApp 
          (wsExceptionHandler (redisConn app) chanSlug) 
            (entityKey c) chanSlug chatUser
        defaultLayout $ do 
          setTitle $ toHtml $ makeTitle slug <> " | TapLike"
          $(widgetFile "chat-room")
      Nothing -> getHomeR

postNewChatR :: Handler ()
postNewChatR = do
    channel <- requireJsonBody :: Handler TP.NewChannel
    authId  <- maybeAuthId
    case authId of
      Just chanCreator -> do
        currentTime <- liftIO getCurrentTime
        let slug  = slugify $ TP.unChannelTitle $ TP.newChannelTitle channel
            topic = TP.unChannelTopic $ TP.newChannelTopic channel
            title = TP.unChannelTitle $ TP.newChannelTitle channel
            color = TP.unChannelColor $ TP.newChannelColor channel
            newChannel = Channel title topic slug currentTime chanCreator color
        runDB (insert newChannel) >>= 
                \key -> sendResponseStatus status201 
                    (toJSON (ChannelCreatedRp 
                              (TP.Channel 
                                (TP.UserId $ fromSqlKey chanCreator) currentTime
                                (TP.ChannelTopic topic) (TP.ChannelSlug $ unSlug slug) 
                                (TP.ChannelTitle title) (TP.NumberUsersPresent 0) 
                                (TP.ChannelColor color)) 
                              (fromSqlKey key) (TP.ChannelSlug $ unSlug slug)))
      Nothing  -> sendResponseStatus status401 ("UNAUTHORIZED" :: Text)

slugify :: Text -> ChannelSlug
slugify = ChannelSlug . makeSlug

makeSlug :: Text -> Text
makeSlug =  replaceAll "[ _]" "-"
          . replaceAll "[^a-z0-9_ ]+" ""
          . T.map toLower
          
makeTitle :: ChannelSlug -> Text 
makeTitle = replaceAll "[-]" " " . T.map toLower . unSlug

getLogOutR :: Handler Html
getLogOutR = clearSession >> getHomeR
