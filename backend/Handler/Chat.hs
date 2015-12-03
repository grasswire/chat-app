{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Chat where

import           Import hiding (toLower)
import           Yesod.WebSockets
import qualified Server as S
import           Network.Wai (remoteHost)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import           Taplike.Shared (RtmEvent(..), TS(..), IncomingMessage(..), ChannelCreatedRp(..))
import qualified Taplike.Shared as SH
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Taplike.ChannelSlug
import           Data.Char (toLower)
import           Data.Text.ICU.Replace
import qualified Database.Esqueleto as E
import qualified Database.Redis as Redis
import           Database.Redis (runRedis, zincrby, zrangeWithscores)
import qualified Data.ByteString.Char8 as C8
import           Data.Binary (encode, decode)
import qualified Data.ByteString as BS
import qualified Data.Aeson as Aeson
import qualified Types as TP
import qualified Data.Map.Strict as MS

import Model.Instances ()

getHealthCheckR :: Handler Text
getHealthCheckR = return "all good"

channelSetKey :: ByteString
channelSetKey = C8.pack "chan_set"

mkChannelSetValue :: Key Channel -> ByteString
mkChannelSetValue  = toStrict . encode . fromSqlKey

chatApp :: (Key Channel) -> Text -> Maybe (Entity User) -> WebSocketsT Handler ()
chatApp channelId channelName userEntity = do
    sendTextData ("Welcome to #" <> channelName)
    app <- getYesod
    (outChan, channel) <- atomically $ do
                chan <- S.lookupOrCreateChannel (chatServer app) channelId
                return (S.channelBroadcastChan chan, chan)
    inChan <- atomically (dupTChan outChan)
    case userEntity of
      Just u -> do
        currentTime <- liftIO getCurrentTime
        void $ lift $ updateLastSeen (entityKey u) currentTime
        liftIO $ atomically $ S.chanAddClient S.JoinReasonConnected channel (userTwitterUserId $ entityVal u)
        liftIO $ runRedis (redisConn app) $ do
          res <- zincrby channelSetKey 1 (mkChannelSetValue channelId)
          print res
        race_
          (ingest inChan)
          (sourceWS $$ mapM_C $ \event -> do
            let outEvent = processMessage u event
            case event of
                 RtmHeartbeat beat -> do
                   currentTime <- liftIO getCurrentTime
                   void $ lift $ updateLastSeen (entityKey u) currentTime
                 _ -> return ()
            atomically $ writeTChan outChan outEvent
         )
      Nothing -> ingest inChan
    where ingest chan                       = forever $ atomically (readTChan chan) >>= sendTextData
          updateLastSeen userId currentTime = runDB (upsert (Heartbeat userId currentTime channelId ) [HeartbeatLastSeen =. currentTime])

processMessage :: Entity User -> RtmEvent -> RtmEvent
processMessage userEntity event = case event of
                              (RtmSendMessage incoming) -> RtmMessage (SH.Message (entityKey userEntity) ( incomingMessageMessageText incoming) (TS "0") (Just $ TS "0") (SH.incomingMessageChannelId incoming))
                              _ -> RtmHello

getUsername :: YesodRequest -> Maybe TL.Text
getUsername req = Just $ TL.pack $ (show . remoteHost . reqWaiRequest) req

newtype RoomId = RoomId Integer

getChatR :: ChannelSlug -> Handler Html
getChatR slug = do
    channel <- runDB (getBy $ UniqueChannelSlug slug)
    authId <- maybeAuthId
    let signature = "chatroom" :: String
    chatUser <- maybe (return Nothing) (\userId -> fmap (Entity userId) <$> runDB (get userId)) authId
    case channel of
      Just c -> do
        let room = entityVal c
        let masthead room = $(widgetFile "partials/chat/masthead")
        let sidebar = $(widgetFile "partials/chat/sidebar")
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
    app <- getYesod
    chansWithScores <- liftIO $ runRedis (redisConn app) $ do
                          chans <- zrangeWithscores channelSetKey 0 8
                          case chans of
                            Left  e -> return []
                            Right r -> return ((\(chanKey, score) -> ((toSqlKey . decode . fromStrict) chanKey, score)) <$> r :: [(Key Channel, Double)])
    let popularChannelIds = fmap fst chansWithScores
    let chanScoreMap = MS.fromList chansWithScores
    chanEntities <- runDB (selectList [ChannelId <-. popularChannelIds] []) :: Handler [Entity Channel]
    let channels = (\c -> chanFromEntity c (fromMaybe (TP.NumberUsersPresent 0) ((TP.NumberUsersPresent . fromIntegral . round) <$> MS.lookup (entityKey c) chanScoreMap))) <$> chanEntities
    authId <- maybeAuthId
    let signature = "home" :: String
    defaultLayout $ do
        setTitle "Taplike / Home"
        $(widgetFile "homepage")

getLogOutR :: Handler Html
getLogOutR = clearSession >> getHomeR
