{-# LANGUAGE TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Auth where

import Import

import qualified Data.ByteString.Char8 as S8
import Web.Twitter.Conduit hiding (lookup, params)
import qualified Web.Authenticate.OAuth as OA
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import qualified Data.Map as M
import qualified Web.Twitter.Types as TT
import Database.Persist.Sql (fromSqlKey)
import Handler.Home (getHomeR)

getLogOutR :: Handler Html
getLogOutR = clearSession >> getHomeR

getRequestToken :: Text -> TwitterConf -> OAuth
getRequestToken callback (TwitterConf _ _ (TwitterConsumerKey consumerKey) (TwitterConsumerSecret secret)) = twitterOAuth
        { oauthConsumerKey = S8.pack $ unpack consumerKey
        , oauthConsumerSecret = S8.pack $ unpack secret
        , oauthCallback = Just $ S8.pack $ unpack callback
        }

storeCredential :: OAuthToken -> Credential -> App -> IO ()
storeCredential k cred app =
    atomicModifyIORef (twitterTokenStore app) $ \m -> (M.insert k cred m, ())

takeCredential :: OAuthToken -> IORef (M.Map OAuthToken Credential) -> IO (Maybe Credential)
takeCredential k ioref =
    atomicModifyIORef ioref $ \m ->
        let (res, newm) = M.updateLookupWithKey (\_ _ -> Nothing) k m in
        (newm, res)

createRoomModalParams :: [(Text, Text)]
createRoomModalParams = [("modal", "create")]

createRedirectParms :: Text -> [(Text, Text)]
createRedirectParms url = [("redirect_url", url)]

getTwitterAuthR :: Handler Text
getTwitterAuthR = do
  app <- getYesod
  let conf = twitterConf . appSettings $ app
  modalParam <- lookupGetParam "modal"
  redirectParam <- lookupGetParam "redirect_url"
  renderFunc <- getUrlRenderParams
  let callbackParams = if isJust modalParam then createRoomModalParams else []
  let callback = renderFunc TwitterCallbackR (maybe callbackParams ((callbackParams ++) . createRedirectParms)  redirectParam)
  let token = getRequestToken callback conf
  manager <- appHttpManager <$> getYesod
  cred <- liftIO $ OA.getTemporaryCredential token manager
  case lookup "oauth_token" $ unCredential cred of
     Just temporaryToken -> do
         liftIO $ storeCredential temporaryToken cred app
         let url = OA.authorizeUrl token cred
         redirect (pack url :: Text)
     Nothing -> redirect (pack "http://disney.com" :: Text)

getTwitterCallbackR :: Handler Html
getTwitterCallbackR = do
   app <- getYesod
   temporaryToken <- lookupGetParam "oauth_token"
   oauthVerifier  <- lookupGetParam "oauth_verifier"
   modalParam     <- lookupGetParam "modal"
   redirectParam  <- lookupGetParam "redirect_url"
   let tokenStore = twitterTokenStore app
       conf = twitterConf . appSettings $ app
       params = if isJust modalParam then createRoomModalParams else []
   renderFunc <- getUrlRenderParams
   let callback = renderFunc TwitterCallbackR params
       homeR = renderFunc HomeR params
       tokens = getRequestToken callback conf
   mcred <- maybe (return Nothing) (liftIO . (flip takeCredential) tokenStore . encodeUtf8) temporaryToken
   case (mcred, oauthVerifier) of
    (Just cred, Just authVer) -> do
          accessTokens <- liftIO $ OA.getAccessToken tokens (OA.insert "oauth_verifier" (encodeUtf8 authVer) cred) (appHttpManager app)
          let token = getRequestToken callback conf
          manager <- appHttpManager <$> getYesod
          user <- liftIO $ verifyTwitterCreds manager (mkTwitterInfo token accessTokens)
          let twitterUserId = fromIntegral $ TT.userId user
          maybePersistedUser <- runDB $ getBy (UniqueUser twitterUserId)
          case maybePersistedUser of
            Nothing -> do
              userId <- liftIO getCurrentTime >>= \ts -> runDB $ insert $ User twitterUserId (TT.userScreenName user) 
                                         (fromMaybe (pack "default-image.png") (TT.userProfileImageURLHttps user)) Nothing ts
              setSession sessionUserIdKey (pack . show $ userId)
              maybe (redirect homeR) redirect redirectParam
            Just u -> do
              setSession sessionUserIdKey (pack . show $ fromSqlKey (entityKey u))
              maybe (redirect homeR) redirect redirectParam
    _ -> redirect homeR

verifyTwitterCreds :: Manager -> TWInfo -> IO TT.User
verifyTwitterCreds manager twInfo = runResourceT (call twInfo manager accountVerifyCredentials)

getUser :: Key User -> Handler (Maybe User)
getUser userKey = runDB $ get userKey

mkTwitterInfo :: OAuth -> Credential -> TWInfo
mkTwitterInfo tokens credential = setCredential tokens credential def

mkCredential :: TwitterToken -> TwitterSecret -> Credential
mkCredential (TwitterToken key) (TwitterSecret secret) = Credential
      [ ("oauth_token", encodeUtf8 key)
      , ("oauth_token_secret", encodeUtf8 secret)]
