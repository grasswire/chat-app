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
import           Database.Persist.Sql (fromSqlKey)
import           Taplike.ChannelSlug
import           Data.Char (toLower)
import           Data.Text.ICU.Replace
import           Database.Redis (runRedis, zincrby)
import qualified Database.Redis as Redis
import qualified Types as TP
import qualified Data.Map.Strict as MS
import Model.Instances ()
import DataStore
import Control.Concurrent (threadDelay, forkIO, ThreadId)
import qualified Control.Exception.Lifted as EL
import Network.WebSockets (ConnectionException)

getHealthCheckR :: Handler Text
getHealthCheckR = return "all good"

type WSExceptionHandler = ConnectionException -> WebSocketsT Handler ()

chatApp :: WSExceptionHandler -> Key Channel -> Text -> Maybe (Entity User) -> WebSocketsT Handler ()
chatApp exceptionHandler channelId channelName userEntity = flip EL.catch exceptionHandler $ do
    sendTextData RtmHello
    sendTextData ("Welcome to #" <> channelName)
    app <- getYesod
    chan   <- liftIO $ S.lookupOrCreateChannel (redisConn app) (chatServer app) channelId
    inChan <- atomically $ dupTChan (S.channelBroadcastChan chan)
    case userEntity of
      Just u -> do
        -- pingThreadId <- liftIO $ pingThread (redisConn app) channelId
        -- pingThreadId <- liftIO (newEmptyMVar :: IO (MVar ThreadId))
        liftIO $ atomically $ S.chanAddClient S.JoinReasonConnected chan (userTwitterUserId $ entityVal u)
        _ <- liftIO $ runRedisAction (redisConn app) $ void (S.broadcastEvent channelId (SH.RtmPresenceChange (SH.PresenceChange (SH.userFromEntity u) SH.PresenceActive)))
        _ <- liftIO $ runRedis (redisConn app) $ zincrby channelPresenceSetKey 1 (mkChannelPresenceSetValue channelId)
        liftIO getCurrentTime >>= \t -> void $ lift $ updateLastSeen (entityKey u) t
        race_
          (ingest inChan)
          (sourceWS $$ mapM_C $ \inEvent ->
            case inEvent of
              RtmHeartbeat _ -> do
                now <- liftIO getCurrentTime
                void $ lift $ updateLastSeen (entityKey u) now
              RtmPing ping -> sendTextData $ RtmPong (SH.Pong $ SH.pingId ping)
              _ -> do
                ackMessage inEvent
                runInnerHandler <- lift (handlerToIO :: HandlerT App IO (HandlerT App IO () -> IO ()))
                _ <- liftIO $ forkIO $ runInnerHandler $ do
                  now <-  liftIO getCurrentTime
                  void $ updateLastSeen (entityKey u) now
                liftIO getCurrentTime >>= (liftIO . runRedisAction (redisConn app) . S.broadcastEvent channelId . processMessage (entityKey u) inEvent) >> return ())
      Nothing -> ingest inChan
    where ingest chan = forever $ atomically (readTChan chan) >>= sendTextData
          updateLastSeen userId currentTime = runDB (upsert (Heartbeat userId currentTime channelId ) [HeartbeatLastSeen =. currentTime])
          ackMessage (RtmSendMessage incoming) = liftIO getCurrentTime >>= \now -> sendTextData (RtmReplyOk (ReplyOk (SH.incomingMessageUUID incoming) (Just now) (Just $ SH.unMessageText $ incomingMessageMessageText incoming)))
          ackMessage _ = return ()

wsExceptionHandler :: Redis.Connection -> ChannelId -> ConnectionException -> WebSocketsT Handler ()
wsExceptionHandler conn chanId e = do
  liftIO $ TIO.putStrLn ("Exception : " <> pack (show e))
  _ <- liftIO $ runRedis conn $ zincrby channelPresenceSetKey (-1) (mkChannelPresenceSetValue chanId)
  return ()

pingThread :: Redis.Connection -> ChannelId -> IO ThreadId
pingThread conn chanId =
    forkIO $ do
      threadDelay thirtySeconds
      putStrLn "thirty seconds expired, decrementing channel presence"
      _ <- runRedis conn $ zincrby channelPresenceSetKey (-1) (mkChannelPresenceSetValue chanId)
      return ()

thirtySeconds :: Int
thirtySeconds = 30000000

processMessage :: UserId -> RtmEvent -> UTCTime -> RtmEvent
processMessage userId event eventTS = case event of
                              (RtmSendMessage incoming) -> RtmMessage (SH.Message userId ( incomingMessageMessageText incoming) (SH.incomingMessageTS incoming) (Just eventTS) (SH.incomingMessageChannelId incoming))
                              _ -> RtmHello

getUsername :: YesodRequest -> Maybe TL.Text
getUsername req = Just $ TL.pack $ (show . remoteHost . reqWaiRequest) req

newtype RoomId = RoomId Integer

getChatR :: ChannelSlug -> Handler Html
getChatR slug = do
    app <- getYesod
    channel <- runDB (getBy $ UniqueChannelSlug slug)
    authId <- maybeAuthId
    renderFunc <- getUrlRenderParams
    let rtmStartUrl = renderFunc RtmStartR [("channel_slug", unSlug slug)]
    let signature = "chatroom" :: Text
    let modalSignin = $(widgetFile "partials/modals/signin")
    chatUser <- maybe (return Nothing) (\userId -> fmap (Entity userId) <$> runDB (get userId)) authId
    case channel of
      Just c -> do
        let room = entityVal c
            masthead = $(widgetFile "partials/chat/masthead")
            sidebar = $(widgetFile "partials/chat/sidebar")
            isLoggedIn = isJust authId
        webSockets $ chatApp (wsExceptionHandler (redisConn app) (entityKey c)) (entityKey c) (channelTitle room) chatUser
        defaultLayout $(widgetFile "chat-room")
      Nothing -> getHomeR

postNewChatR :: Handler ()
postNewChatR = do
    channel <- requireJsonBody :: Handler TP.NewChannel
    authId  <- maybeAuthId
    case authId of
      Just chanCreator -> do
        currentTime <- liftIO getCurrentTime
        let slug = slugify $ TP.unNewChannelTitle $ TP.newChannelTitle channel
            newChannel  = Channel (TP.unNewChannelTitle $ TP.newChannelTitle channel) (TP.unNewChannelTopic $ TP.newChannelTopic channel) slug currentTime chanCreator
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
    authId <- maybeAuthId
    app <- getYesod
    let signature = "home" :: String
    let modalCreate = $(widgetFile "partials/modals/create")
    chansWithScores <- liftIO $ runRedisAction (redisConn app) (channelsByPresenceDesc 27)
    (topChannels, allChannels) <- case chansWithScores of
      Right cs -> do
        let filtered = filter ((>= 1) . TP.unNumberUsersPresent . snd) cs
            popularChannelIds = fmap fst filtered
            chanScoreMap = MS.fromList filtered
        chanEntities <- runDB (selectList [ChannelId <-. popularChannelIds] []) :: Handler [Entity Channel]
        return $ splitAt 9 $ sortBy (flip compare `on` TP.channelNumUsersPresent ) $ (\c -> chanFromEntity c (fromMaybe (TP.NumberUsersPresent 0) (MS.lookup (entityKey c) chanScoreMap))) <$> chanEntities
      Left _ -> return ([], [])
    defaultLayout $ do
      setTitle "Taplike / Home"
      $(widgetFile "homepage")

getLogOutR :: Handler Html
getLogOutR = clearSession >> getHomeR
