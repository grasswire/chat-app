{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Chat where

import           Text.Julius
import           Import hiding (toLower)
import           Yesod.WebSockets
import qualified Server as S
import qualified Data.Text as T
import           Types (RtmEvent(..), IncomingMessage(..), ChannelCreatedRp(..), ReplyOk(..))
import           Taplike.Shared (userFromEntity)
import           Database.Persist.Sql (fromSqlKey)
import           Taplike.Schema
import           Data.Char (toLower)
import           Data.Text.ICU.Replace
import qualified Database.Redis as Redis
import qualified Types as TP
import           Model.Instances ()
import           DataStore
import           Control.Concurrent (forkIO)
import qualified Control.Exception.Lifted as EL
import           Network.WebSockets (ConnectionException)
import           Handler.Home (getHomeR)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import Queries 

data ChatSettings = AnonymousSettings AnonymousChat | LoggedInSettings LoggedInChat 

data AnonymousChat = AnonymousChat { anonymousChatChannel :: Entity Channel }

data LoggedInChat = LoggedInChat 
  { loggedInChatUser     :: Entity User 
  , loggedInChatChannels :: NonEmpty (Entity Channel)
  }
  
settingsSlugs :: ChatSettings -> NonEmpty ChannelSlug
settingsSlugs settings =  (channelCrSlug . entityVal) <$> settingsChannels settings

settingsChannels :: ChatSettings -> NonEmpty (Entity Channel)
settingsChannels (AnonymousSettings settings) =  anonymousChatChannel settings :| []
settingsChannels (LoggedInSettings settings)  =  loggedInChatChannels settings

type WSExceptionHandler = ConnectionException -> WebSocketsT Handler ()

addMember :: Key Channel -> Key User -> Handler (Either (Entity Membership) (Key Membership))
addMember channelId user = do 
    now    <- liftIO getCurrentTime 
    runDB $ insertBy (Membership user channelId now True)
    
getChatR :: ChannelSlug -> Handler Html
getChatR slug = do
    app <- getYesod
    channel <- runDB (getBy $ UniqueChannelSlug slug)
    authId <- maybeAuthId
    renderFunc <- getUrlRenderParams
    chatUser <- maybe (return Nothing) (\userId -> fmap (Entity userId) <$> runDB (get userId)) authId
    let rtmStartUrl           = renderFunc RtmStartR [("channel_slug", unSlug slug)]
        signature             = "chatroom" :: Text
        htmlSlug              = unSlug slug
        modalSignin           = $(widgetFile "partials/modals/signin")
        redirectUrl           = renderFunc (ChatR slug) []
        loginWithChatRedirect = renderFunc TwitterAuthR [("redirect_url", redirectUrl)]
    case channel of
      Just c -> do
        let room = entityVal c
            isLoggedIn = isJust authId
            chanSlug = channelCrSlug room
        chatSettings <- case chatUser of 
          Just user -> do 
            usersChans <- runDB (usersChannels (entityKey user))
            case usersChans of 
              h:t -> return (LoggedInSettings (LoggedInChat user (h :| t)))
              _ ->   return (AnonymousSettings (AnonymousChat c))
          Nothing -> return (AnonymousSettings (AnonymousChat c)) 
        webSockets $ socketsApp (wsExceptionHandler (redisConn app) chanSlug) chatSettings
        defaultLayout $ do
          setTitle $ toHtml $ makeTitle slug <> " | TapLike"
          $(widgetFile "chat-room")
      Nothing -> getHomeR

socketsApp :: WSExceptionHandler -> ChatSettings -> WebSocketsT Handler ()
socketsApp exceptionHandler settings =
  flip EL.catch exceptionHandler $ do
    sendTextData RtmHello
    app    <- getYesod
    client <- liftIO S.newClient
    _      <- liftIO $ S.subscribe (settingsSlugs settings) client (chatServer app)
    case settings of
      LoggedInSettings s -> race_ (outbound $ S.readMessage client) (inbound app (loggedInChatUser s))
      _                  ->        outbound $ S.readMessage client
  where
    outbound readMessages = forever $ liftIO readMessages >>= sendTextData
    inbound (App { redisConn }) user = do
      -- let wasSeen = do now <- liftIO getCurrentTime
      --                  void $ runDB (upsert (Heartbeat (entityKey user) now channelId) [HeartbeatLastSeen =. now])
      -- 
      liftIO $ 
        -- atomically $ S.chanAddClient S.JoinReasonConnected chan
        --             (userTwitterUserId $ entityVal user)
        void . runRedisAction redisConn $ S.broadcastEvent (ChannelSlug "haskell")
                 (TP.RtmPresenceChange (TP.PresenceChange (userFromEntity user) TP.PresenceActive))
      --   void . runRedisAction redisConn $ incrChannelPresence channelSlug
      -- 
      -- lift wasSeen
      
      runInnerHandler <- lift handlerToIO
      
      -- liftIO $ runInnerHandler $ void $ addMember channelId (entityKey user)

      sourceWS $$ mapM_C $ \ case
        RtmHeartbeat _ -> return () -- lift wasSeen
        RtmPing ping   -> sendTextData $ RtmPong (TP.Pong $ TP.pingId ping)
        inEvent -> do
          ackMessage inEvent
          void $ liftIO $ forkIO $ runInnerHandler $ do
            -- wasSeen
            ts <- liftIO getCurrentTime
            let processedMsg = processMessage (entityKey user) inEvent ts
            case processedMsg of 
              Just event -> do 
                _ <- liftIO $ runRedisAction redisConn $ S.broadcastEvent (NonEmpty.head $ settingsSlugs settings) event
                persistEvent (entityKey user) event
              Nothing    -> return ()
          pure ()

    persistEvent :: UserId -> RtmEvent -> Handler ()
    persistEvent userId event = case event of
                                  RtmMessage msg -> void $ runDB $ insert 
                                    (Message userId (TP.unMessageText $ TP.messageText msg) 
                                                    (TP.messageTS msg) (entityKey (NonEmpty.head $ settingsChannels settings)) (MessageUUID $ TP.messageUUID msg))
                                  _              -> return ()

    ackMessage (RtmSendMessage incoming) = do
      now <- liftIO getCurrentTime
      sendTextData (RtmReplyOk (ReplyOk (TP.incomingMessageUUID incoming) (Just now) (Just $ TP.unMessageText $ incomingMessageMessageText incoming)))
    ackMessage _ = return ()

wsExceptionHandler :: Redis.Connection -> ChannelSlug -> ConnectionException -> WebSocketsT Handler ()
wsExceptionHandler conn channelSlug e = do
  _ <- liftIO . void . runRedisAction conn $ decrChannelPresence channelSlug
  throwM e

processMessage :: UserId -> RtmEvent -> UTCTime -> Maybe RtmEvent
processMessage userId event eventTS = case event of
                               RtmSendMessage incoming  -> Just $ 
                                 RtmMessage (TP.Message (TP.UserId $ fromSqlKey userId ) (incomingMessageMessageText incoming) 
                                          (TP.incomingMessageTS incoming) (Just eventTS) (TP.incomingMessageChannelId incoming) 
                                          (TP.incomingMessageUUID incoming) [])
                               _                        -> Nothing

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
                    (toJSON (ChannelCreatedRp (TP.Channel (TP.UserId $ fromSqlKey chanCreator) currentTime
                                (TP.ChannelTopic topic) (TP.ChannelSlug $ unSlug slug)
                                (TP.ChannelTitle title) (TP.NumberUsersPresent 0)
                                (TP.ChannelColor color) []) (fromSqlKey key) (TP.ChannelSlug $ unSlug slug)))
      Nothing  -> sendResponseStatus status401 ("UNAUTHORIZED" :: Text)

slugify :: Text -> ChannelSlug
slugify = ChannelSlug . makeSlug

makeSlug :: Text -> Text
makeSlug =  replaceAll "[ _]" "-"
          . replaceAll "[^a-z0-9_ ]+" ""
          . T.map toLower

makeTitle :: ChannelSlug -> Text
makeTitle = replaceAll "[-]" " " . T.map toLower . unSlug