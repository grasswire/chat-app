{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Chat where
import           Text.Julius
import           Import hiding (toLower)
import           Yesod.WebSockets
import qualified Server as S
import           Network.Wai (remoteHost)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import           Taplike.Shared (RtmEvent(..), TS(..), IncomingMessage(..), ChannelCreatedRp(..), ReplyOk(..), MessageText)
import qualified Taplike.Shared as SH
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Taplike.ChannelSlug
import           Data.Char (toLower)
import           Data.Text.ICU.Replace
import           Database.Redis (runRedis, zincrby, zrangeWithscores)
import qualified Database.Redis as Redis
import           Data.Binary (decode)
import qualified Data.ByteString as BS
import qualified Types as TP
import qualified Data.Map.Strict as MS
import Model.Instances ()
import DataStore
import Control.Concurrent (threadDelay, forkIO, ThreadId)
import qualified Control.Exception.Lifted as EL

getHealthCheckR :: Handler Text
getHealthCheckR = return "all good"

chatApp :: Key Channel -> Text -> Maybe (Entity User) -> WebSocketsT Handler ()
chatApp channelId channelName userEntity = (flip EL.catch) wsExceptionHandler $ do
    $(logInfo) "Running WebSocketsT Handler"
    sendTextData RtmHello
    sendTextData ("Welcome to #" <> channelName)
    app    <- getYesod
    chan   <- liftIO $ S.lookupOrCreateChannel (redisConn app) (chatServer app) channelId
    inChan <- atomically $ dupTChan (S.channelBroadcastChan chan)
    case userEntity of
      Just u -> do
        pingThreadId <- liftIO $ pingThread (redisConn app) channelId
        pingThreadId <- liftIO (newEmptyMVar :: IO (MVar ThreadId))
        liftIO $ atomically $ S.chanAddClient S.JoinReasonConnected chan (userTwitterUserId $ entityVal u)
        _ <- liftIO $ runRedis (redisConn app) $ zincrby channelPresenceSetKey 1 (mkChannelPresenceSetValue channelId)
        liftIO getCurrentTime >>= \t -> void $ lift $ updateLastSeen (entityKey u) t
        race_
          (ingest inChan)
          (sourceWS $$ mapM_C $ \inEvent ->
            case inEvent of
              RtmHeartbeat beat -> do
                now <- liftIO getCurrentTime
                void $ lift $ updateLastSeen (entityKey u) now
              RtmPing ping -> sendTextData $ RtmPong (SH.Pong $ SH.pingId ping)
              _ -> do
                ackMessage inEvent
                liftIO getCurrentTime >>= (liftIO . runRedisAction (redisConn app) . S.broadcastEvent channelId . processMessage (entityKey u) inEvent) >> return ())
      Nothing -> ingest inChan
    where ingest chan = forever $ atomically (readTChan chan) >>= sendTextData
          updateLastSeen userId currentTime = runDB (upsert (Heartbeat userId currentTime channelId ) [HeartbeatLastSeen =. currentTime])
          ackMessage (RtmSendMessage incoming) = liftIO getCurrentTime >>= \now -> sendTextData (RtmReplyOk (ReplyOk (SH.incomingMessageUUID incoming) (Just now) (Just $ SH.unMessageText $ incomingMessageMessageText incoming)))
          ackMessage _ = return ()

wsExceptionHandler :: SomeException -> WebSocketsT Handler ()
wsExceptionHandler _ = liftIO $ putStrLn "got some exception!!"

pingThread :: Redis.Connection -> ChannelId -> IO ThreadId
pingThread conn chanId =
    forkIO $ do
      threadDelay thirtySeconds
      putStrLn "thirty seconds expired, decrementing channel presence"
      runRedis conn $ zincrby channelPresenceSetKey (-1) (mkChannelPresenceSetValue chanId)
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
        let masthead = $(widgetFile "partials/chat/masthead")
        let sidebar = $(widgetFile "partials/chat/sidebar")
        let isLoggedIn = isJust authId
        webSockets $ chatApp (entityKey c) (channelTitle room) chatUser
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
        let popularChannelIds = fmap fst cs
        let chanScoreMap = MS.fromList cs
        chanEntities <- runDB (selectList [ChannelId <-. popularChannelIds] []) :: Handler [Entity Channel]
        return $ splitAt 9 $ sortBy (flip compare `on` TP.channelNumUsersPresent ) $ (\c -> chanFromEntity c (fromMaybe (TP.NumberUsersPresent 0) (MS.lookup (entityKey c) chanScoreMap))) <$> chanEntities
      Left e -> return ([], [])
    defaultLayout $ do
      setTitle "Taplike / Home"
      $(widgetFile "homepage")

getLogOutR :: Handler Html
getLogOutR = clearSession >> getHomeR
