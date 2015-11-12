{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Handler.Home where

import Import
import Yesod.WebSockets
import Server
import Network.Wai (remoteHost)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as S8
import Web.Twitter.Conduit hiding (lookup)
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import qualified Data.ByteString as S

getHealthCheckR :: Handler Text
getHealthCheckR = return "all good"

callback :: String
callback = "http://localhost:3000/callback"

-- getRequestToken :: TwitterConf -> IO OAuth
-- getRequestToken conf = do
--     consumerKey <- consumerKey conf
--     consumerSecret <- consumerSecret conf
--     return $
--         twitterOAuth
--         { oauthConsumerKey = S8.pack consumerKey
--         , oauthConsumerSecret = S8.pack consumerSecret
--         , oauthCallback = Just $ S8.pack callback
--         }

type OAuthToken = S.ByteString

getTwitterAuthR :: Handler Text
getTwitterAuthR = do
  -- conf <- twitterConf . appSettings <$> getYesod
  -- token <- getRequestToken conf
  return "hi"
  -- return redirect "https://api.twitter.com/oauth/authenticate?oauth_token=" <>


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

-- placeholder for user auth/fetching username stuff
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
    let chatRooms = [ChatRoom "NFL showdown" "all things foootball", ChatRoom "sunday funday" "chill on a sunday", ChatRoom "Rangers Rant" "Live! Let's talk about the game tonight", ChatRoom "Tinfoil" "The earth is hollow! We all know it's true so lets discuss"]
    defaultLayout $ do
        setTitle "Taplike / Home"
        $(widgetFile "homepage")
