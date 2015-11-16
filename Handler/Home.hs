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
import qualified Data.ByteString as S
import qualified Data.Map as M
import qualified Network.HTTP.Conduit as HTTP
import qualified Web.Twitter.Types as TT
import Database.Persist.TH
import qualified Data.Text.IO as TIO
import Data.Int
import Database.Persist.Types()
import Database.Persist.Sql (toSqlKey)


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

makeMessage :: OAuth -> Credential -> S.ByteString
makeMessage tokens (Credential cred) =
    S8.intercalate "\n"
        [ "export OAUTH_CONSUMER_KEY=\"" <> oauthConsumerKey tokens <> "\""
        , "export OAUTH_CONSUMER_SECRET=\"" <> oauthConsumerSecret tokens <> "\""
        , "export OAUTH_ACCESS_TOKEN=\"" <> fromMaybe "" (lookup "oauth_token" cred) <> "\""
        , "export OAUTH_ACCESS_SECRET=\"" <> fromMaybe "" (lookup "oauth_token_secret" cred) <> "\""
        ]

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

getTwitterCallbackR :: Handler Text
getTwitterCallbackR = do
   app <- getYesod
   temporaryToken <- lookupGetParam "oauth_token"
   oauthVerifier <-  lookupGetParam "oauth_verifier"
   let tokenStore = twitterTokenStore app
   let conf = twitterConf . appSettings $ app
   renderFunc <- getUrlRender
   let callback = renderFunc $ TwitterCallbackR
   let tokens = getRequestToken callback conf
   mcred <- case temporaryToken of
              Just t -> liftIO $ takeCredential (encodeUtf8 t) tokenStore
              Nothing -> return Nothing
   case mcred of
    Just cred -> do
      case oauthVerifier of
        Just authVer -> do
          accessTokens <- liftIO $ HTTP.withManager $ OA.getAccessToken tokens (OA.insert "oauth_verifier" (encodeUtf8 authVer) cred)
          renderFunc <- getUrlRender
          let callback = renderFunc $ TwitterCallbackR
          let token = getRequestToken callback conf
          user <- liftIO $ verifyTwitterCreds $ mkTwitterInfo token accessTokens
          maybePersistedUser <- getUser (fromIntegral $ TT.userId user )
          case maybePersistedUser of
            Nothing -> do
              bearerToken <- genBearerToken user
              setSession "twitter-user-id" (pack . show $ TT.userId user)
              setSession "Bearer-Token" (decodeUtf8 bearerToken)
              setSession "twitter-profile-image-url" (fromMaybe (pack "default image") (TT.userProfileImageURLHttps user))
              bToken <- lookupSession "Bearer-Token"
              case bToken of
                Just t -> liftIO $ TIO.putStrLn t >> return "ok"
                Nothing -> liftIO $ putStrLn "shit" >> return "not ok"
            Just u -> return "ok"
          -- return (pack . show $ user)
        Nothing -> return "temporary token is not found"
    Nothing -> return "temporary token is not found"

verifyTwitterCreds :: TWInfo -> IO TT.User
verifyTwitterCreds twInfo =  do
  manager <- HTTP.newManager defaultManagerSettings
  runResourceT (call twInfo manager accountVerifyCredentials)

type BearerToken = ByteString

genBearerToken :: TT.User -> Handler BearerToken
genBearerToken twttrUser = return $ S8.pack "random BearerToken"

getUser :: Int64 -> Handler (Maybe User)
getUser twttrUserId = return Nothing -- runDB $ get $ (toSqlKey twttrUserId :: Key User)

mkTwitterInfo :: OAuth -> Credential -> TWInfo
mkTwitterInfo tokens credential = setCredential tokens credential def

mkCredential :: TwitterToken -> TwitterSecret -> Credential
mkCredential (TwitterToken key) (TwitterSecret secret) = Credential
      [ ("oauth_token", encodeUtf8 key)
      , ("oauth_token_secret", encodeUtf8 secret)]

chatApp :: Text -> WebSocketsT Handler ()
chatApp channelName = do
    sendTextData ("Welcome to #" <> channelName)
-- name <- receiveData :: IO Message
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

data ChatRoom = ChatRoom { title :: Text
                         , description :: Text}

getChatR :: Text -> Handler Html
getChatR roomId = do
    webSockets $ chatApp roomId
    defaultLayout $ do
        $(widgetFile "chat-room")

getHomeR :: Handler Html
getHomeR = do
    let chatRooms = [ChatRoom "NFL showdown" "all things foootball", ChatRoom "sunday funday" "chill on a sunday"]
    defaultLayout $ do
        setTitle "Taplike / Home"
        $(widgetFile "homepage")
