{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Chat where
import           Text.Julius
import           Import hiding (toLower)
import           Yesod.WebSockets
import qualified Server as S
import           Network.Wai (remoteHost)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Taplike.Shared (RtmEvent(..), IncomingMessage(..), ChannelCreatedRp(..), ReplyOk(..))
import qualified Taplike.Shared as SH
import           Database.Persist.Sql (fromSqlKey, rawSql)
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
import Data.Time.Clock


getHealthCheckR :: Handler Text
getHealthCheckR = return "all good!!"

type WSExceptionHandler = ConnectionException -> WebSocketsT Handler ()

chatApp :: WSExceptionHandler -> Key Channel -> ChannelSlug -> Maybe (Entity User) -> WebSocketsT Handler ()
chatApp exceptionHandler channelId channelSlug userEntity =
  flip EL.catch exceptionHandler $ do
    sendTextData RtmHello
    app <- getYesod
    chan   <- liftIO $ S.lookupOrCreateChannel (redisConn app) (chatServer app) channelSlug
    toSend <- atomically $ dupTChan (S.channelBroadcastChan chan)
    case userEntity of
      Just u  -> race_ (outbound toSend) (inbound app u chan)
      Nothing ->        outbound toSend
  where
    outbound chan = forever $ atomically (readTChan chan) >>= sendTextData
    inbound (App { redisConn }) user chan = do
      let wasSeen = do now <- liftIO getCurrentTime
                       void $ runDB (upsert (Heartbeat (entityKey user) now channelId) [HeartbeatLastSeen =. now])

      void $ liftIO $ do
        atomically $ S.chanAddClient S.JoinReasonConnected chan (userTwitterUserId $ entityVal user)
        void . runRedisAction redisConn $ S.broadcastEvent channelSlug (SH.RtmPresenceChange (SH.PresenceChange (SH.userFromEntity user) SH.PresenceActive))
        void . runRedisAction redisConn $ incrChannelPresence channelSlug

      lift wasSeen

      sourceWS $$ mapM_C $ \ case
        RtmHeartbeat _ -> lift wasSeen
        RtmPing ping -> sendTextData $ RtmPong (SH.Pong $ SH.pingId ping)
        inEvent -> do
          ackMessage inEvent
          runInnerHandler <- lift handlerToIO
          void $ liftIO $ forkIO $ runInnerHandler $ do
            wasSeen
            ts <- liftIO getCurrentTime
            let processedMsg = processMessage (entityKey user) inEvent ts
            maybe (return ()) (\e -> void (liftIO $ runRedisAction redisConn $ S.broadcastEvent channelSlug e) >> persistEvent (entityKey user) e) processedMsg
          pure ()

    persistEvent :: UserId -> RtmEvent -> Handler ()
    persistEvent userId event = case event of
                                  RtmMessage msg -> void $ runDB $ insert (Message userId (SH.unMessageText $ SH.messageText msg) (SH.messageTS msg) channelId (MessageUUID $ SH.messageUUID msg))
                                  _              -> return ()

    ackMessage (RtmSendMessage incoming) = do
      now <- liftIO getCurrentTime
      sendTextData (RtmReplyOk (ReplyOk (SH.incomingMessageUUID incoming) (Just now) (Just $ SH.unMessageText $ incomingMessageMessageText incoming)))
    ackMessage _ = return ()

wsExceptionHandler :: Redis.Connection -> ChannelSlug -> ConnectionException -> WebSocketsT Handler ()
wsExceptionHandler conn channelSlug e = do
  liftIO $ TIO.putStrLn ("Exception : " <> pack (show e))
  _ <- liftIO . void . runRedisAction conn $ decrChannelPresence channelSlug
  return ()

processMessage :: UserId -> RtmEvent -> UTCTime -> Maybe RtmEvent
processMessage userId event eventTS = case event of
                              (RtmSendMessage incoming) -> Just $ RtmMessage (SH.Message userId ( incomingMessageMessageText incoming) (SH.incomingMessageTS incoming) (Just eventTS) (SH.incomingMessageChannelId incoming) (SH.incomingMessageUUID incoming))
                              _                         -> Nothing

getUsername :: YesodRequest -> Maybe TL.Text
getUsername req = Just $ TL.pack $ (show . remoteHost . reqWaiRequest) req

getChatR :: ChannelSlug -> Handler Html
getChatR slug = do
    app <- getYesod
    channel <- runDB (getBy $ UniqueChannelSlug slug)
    authId <- maybeAuthId
    renderFunc <- getUrlRenderParams
    let rtmStartUrl = renderFunc RtmStartR [("channel_slug", unSlug slug)]
        signature = "chatroom" :: Text
        modalSignin = $(widgetFile "partials/modals/signin")
    chatUser <- maybe (return Nothing) (\userId -> fmap (Entity userId) <$> runDB (get userId)) authId
    case channel of
      Just c -> do
        let room = entityVal c
            isLoggedIn = isJust authId
            chanSlug = channelCrSlug room
        webSockets $ chatApp (wsExceptionHandler (redisConn app) chanSlug) (entityKey c) chanSlug chatUser
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
        let slug = slugify $ TP.unChannelTitle $ TP.newChannelTitle channel
            newChannel  = Channel (TP.unChannelTitle $ TP.newChannelTitle channel) (TP.unChannelTopic $ TP.newChannelTopic channel) slug currentTime chanCreator (TP.unChannelColor $ TP.newChannelColor channel)
        runDB (insert newChannel) >>= \key -> sendResponseStatus status201 (toJSON (ChannelCreatedRp newChannel (fromSqlKey key) slug))
      Nothing  -> sendResponseStatus status401 ("UNAUTHORIZED" :: Text)

slugify :: Text -> ChannelSlug
slugify = ChannelSlug . makeSlug

makeSlug :: Text -> Text
makeSlug =  replaceAll "[ _]" "-"
          . replaceAll "[^a-z0-9_ ]+" ""
          . T.map toLower
          
makeTitle :: ChannelSlug -> Text 
makeTitle = replaceAll "[-]" " " . T.map toLower . unSlug

getHomeR :: Handler Html
getHomeR = do
    authId <- maybeAuthId
    app <- getYesod
    let signature = "home" :: String
    let modalCreate = $(widgetFile "partials/modals/create")
    timeNow <- liftIO getCurrentTime
    let minActiveAgo = addUTCTime (negate 3600 :: NominalDiffTime) timeNow
    (topChannels, allChannels) <- do
        chanEntities <- runDB (popularChannels minActiveAgo)
        presences <- liftIO $ runRedisAction (redisConn app) $ getPresenceForChannels (channelCrSlug . entityVal <$> chanEntities)
        let zipped = case presences of
                      Right ps -> chanEntities `zip` ps
                      Left _   -> chanEntities `zip` replicate (length chanEntities) (TP.NumberUsersPresent 0)
        return $ splitAt 9 $ sortBy (flip compare `on` TP.channelNumUsersPresent ) $ (uncurry chanFromEntity) <$> zipped

    defaultLayout $ do
      setTitle "Taplike / Home"
      $(widgetFile "homepage")

popularChannelsStatement :: Text
popularChannelsStatement = "select ?? from channel where id in (select channel from message where timestamp >= ? group by channel order by count(*) desc limit 27);"

popularChannels :: MonadIO m => UTCTime -> ReaderT SqlBackend m [Entity Channel]
popularChannels since = rawSql popularChannelsStatement [PersistUTCTime since]

getLogOutR :: Handler Html
getLogOutR = clearSession >> getHomeR
