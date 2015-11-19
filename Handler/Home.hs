{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Home where

import Import
import Yesod.WebSockets
import Server

import Network.Wai (remoteHost)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as S8
import Web.Twitter.Conduit hiding (lookup)
import qualified Web.Authenticate.OAuth as OA
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import qualified Data.Map as M
import qualified Network.HTTP.Conduit as HTTP
import qualified Web.Twitter.Types as TT
import qualified Model.Incoming as Incoming
import Data.Maybe (fromMaybe)
import Data.Aeson (decode)

getHealthCheckR :: Handler Text
getHealthCheckR = return "all good"

getRequestToken :: Text -> TwitterConf -> OAuth
getRequestToken callback (TwitterConf _ _ (TwitterConsumerKey consumerKey) (TwitterConsumerSecret secret)) = twitterOAuth
        { oauthConsumerKey = S8.pack $ unpack consumerKey
        , oauthConsumerSecret = S8.pack $ unpack secret
        , oauthCallback = Just $ S8.pack $ unpack $ callback
        }

storeCredential :: OAuthToken -> Credential -> App -> IO ()
storeCredential k cred app =
    atomicModifyIORef (twitterTokenStore app) $ \m -> (M.insert k cred m, ())

takeCredential :: OAuthToken -> IORef (M.Map OAuthToken Credential) -> IO (Maybe Credential)
takeCredential k ioref =
    atomicModifyIORef ioref $ \m ->
        let (res, newm) = M.updateLookupWithKey (\_ _ -> Nothing) k m in
        (newm, res)

getTwitterAuthR :: Handler Text
getTwitterAuthR = do
  app <- getYesod
  let conf = twitterConf . appSettings $ app
  renderFunc <- getUrlRender
  let callback = renderFunc $ TwitterCallbackR
  let token = getRequestToken callback conf
  cred <- liftIO $ HTTP.withManager $ OA.getTemporaryCredential token
  case lookup "oauth_token" $ unCredential cred of
     Just temporaryToken -> do
         liftIO $ storeCredential temporaryToken cred app
         let url = OA.authorizeUrl token cred
         redirect $  (pack url :: Text)
     Nothing -> redirect (pack "http://disney.com" :: Text)

getTwitterCallbackR :: Handler Html
getTwitterCallbackR = do
   app <- getYesod
   temporaryToken <- lookupGetParam "oauth_token"
   oauthVerifier <-  lookupGetParam "oauth_verifier"
   let tokenStore = twitterTokenStore app
   let conf = twitterConf . appSettings $ app
   renderFunc <- getUrlRender
   let callback = renderFunc $ TwitterCallbackR
   let homeR = renderFunc $ HomeR
   let tokens = getRequestToken callback conf
   mcred <- case temporaryToken of
              Just t -> liftIO $ takeCredential (encodeUtf8 t) tokenStore
              Nothing -> return Nothing
   case (mcred, oauthVerifier) of
    (Just cred, Just authVer) -> do
          accessTokens <- liftIO $ HTTP.withManager $ OA.getAccessToken tokens (OA.insert "oauth_verifier" (encodeUtf8 authVer) cred)
          let token = getRequestToken callback conf
          user <- liftIO $ verifyTwitterCreds $ mkTwitterInfo token accessTokens
          let twitterUserId = (fromIntegral $ TT.userId user)
          maybePersistedUser <- getUser $ UserKey twitterUserId
          case maybePersistedUser of
            Nothing -> do
              userId <- runDB $ insert $ User twitterUserId (TT.userName user) (fromMaybe (pack "default-image.png") (TT.userProfileImageURLHttps user)) Nothing
              setSession sessionUserIdKey (pack . show $ userId)
              redirect homeR
            Just u -> do
              setSession sessionUserIdKey (pack . show $ userTwitterUserId u)
              redirect homeR
    _ -> redirect homeR

verifyTwitterCreds :: TWInfo -> IO TT.User
verifyTwitterCreds twInfo =  do
  manager <- HTTP.newManager defaultManagerSettings
  runResourceT (call twInfo manager accountVerifyCredentials)

getUser :: Key User -> Handler (Maybe User)
getUser userKey = runDB $ get $ userKey

mkTwitterInfo :: OAuth -> Credential -> TWInfo
mkTwitterInfo tokens credential = setCredential tokens credential def

mkCredential :: TwitterToken -> TwitterSecret -> Credential
mkCredential (TwitterToken key) (TwitterSecret secret) = Credential
      [ ("oauth_token", encodeUtf8 key)
      , ("oauth_token_secret", encodeUtf8 secret)]

chatApp :: Text -> Maybe User -> WebSocketsT Handler ()
chatApp channelName user = do
    sendTextData ("Welcome to #" <> channelName)
    hostname <- getUsername <$> getRequest
    case hostname of
      Just clientHost -> do
          app <- getYesod
          outChan <- atomically $ (channelBroadcastChan <$> lookupOrCreateChannel (chatServer app) (fromStrict channelName))
          inChan <- atomically $ do
              dupTChan outChan
          race_
              (forever $ atomically (readTChan inChan) >>= sendTextData)
              (sourceWS $$ mapM_C (\msg ->
                  atomically $ writeTChan outChan $  msg))
      Nothing -> notAuthenticated

getUsername :: YesodRequest -> Maybe TL.Text
getUsername req = Just $ TL.pack $ (show . remoteHost . reqWaiRequest) req

newtype RoomId = RoomId Integer

getChatR :: Key ChatRoom -> Handler Html
getChatR chatId = do
    chatRoom <- runDB (get chatId)
    authId <- maybeAuthId
    chatUser <- case authId of
                  Just uId -> runDB $ get uId
                  _        -> return Nothing
    case chatRoom of
      Just chat -> do
        webSockets $ chatApp (chatRoomTitle chat) chatUser
        defaultLayout $ do
          $(widgetFile "chat-room")
      Nothing -> getHomeR

postNewChatR :: Handler ()
postNewChatR = do
    chatRoom <- requireJsonBody :: Handler Incoming.ChatRoom
    authId <- maybeAuthId
    case authId of
      Just i -> runDB (insert (ChatRoom i (Incoming.title chatRoom) (Incoming.description chatRoom))) >> sendResponseStatus status201 ("CREATED" :: Text)
      Nothing  -> sendResponseStatus status401 ("UNAUTHORIZED" :: Text)


getHomeR :: Handler Html
getHomeR = do
    chatRooms <- runDB $ (selectList [] [LimitTo 5]) :: Handler [Entity ChatRoom]
    renderer <- getUrlRenderParams
    defaultLayout $ do
        setTitle "Taplike / Home"
        $(widgetFile "homepage")

getLogOutR :: Handler Html
getLogOutR = clearSession >> getHomeR
