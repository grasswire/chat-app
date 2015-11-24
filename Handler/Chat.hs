{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Chat where

import Import hiding (toLower)
import Yesod.WebSockets
import Server

import Network.Wai (remoteHost)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Model.Incoming as Incoming
import Taplike.Shared (RtmEvent(..), Message(..), TS(..), IncomingMessage(..), ChatRoomCreatedRp(..))
import Database.Persist.Sql (fromSqlKey)
import Taplike.ChatRoomSlug
import Data.Char (isAlphaNum, toLower)
import Text.Regex.PCRE ( (=~~) )
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text.Lazy.Encoding as TLE



getHealthCheckR :: Handler Text
getHealthCheckR = return "all good"

chatApp :: Text -> Maybe User -> WebSocketsT Handler ()
chatApp channelName user = do
    sendTextData ("Welcome to #" <> channelName)
    app <- getYesod
    (outChan, channel) <- atomically $ do
                chan <- lookupOrCreateChannel (chatServer app) (fromStrict channelName)
                return (channelBroadcastChan chan, chan)
    inChan <- atomically (dupTChan outChan)
    case user of
      Just u -> do
        _ <- liftIO $ atomically $ chanAddClient JoinReasonConnected channel (userTwitterUserId u)
        race_
          (ingest inChan)
          (sourceWS $$ mapM_C (atomically . writeTChan outChan . processMessage u))
      Nothing -> ingest inChan
    where ingest chan = forever $ atomically (readTChan chan) >>= sendTextData

processMessage :: User -> RtmEvent -> RtmEvent
processMessage user event = case event of
                              (RtmSendMessage incoming) -> RtmMessage (Message (userTwitterUserId user) Nothing ( incomingMessageMessageText incoming) (TS "0") Nothing Nothing Nothing False Nothing [])
                              _ -> RtmHello

getUsername :: YesodRequest -> Maybe TL.Text
getUsername req = Just $ TL.pack $ (show . remoteHost . reqWaiRequest) req

newtype RoomId = RoomId Integer

getChatR :: ChatRoomSlug -> Handler Html
getChatR slug = do
    chatRoom <- runDB (getBy $ UniqueChatRoomSlug slug)
    authId <- maybeAuthId
    chatUser <- case authId of
                  Just uId -> runDB $ get uId
                  _        -> return Nothing
    case chatRoom of
      Just chat -> do
        webSockets $ chatApp (chatRoomTitle $ entityVal chat) chatUser
        defaultLayout $(widgetFile "chat-room")
      Nothing -> getHomeR

postNewChatR :: Handler ()
postNewChatR = do
    chatRoom <- requireJsonBody :: Handler Incoming.ChatRoom
    authId <- maybeAuthId
    case authId of
      Just i -> do
        let slug = slugify $ Incoming.title chatRoom
        let newRoom = ChatRoom i (Incoming.title chatRoom) (Incoming.description chatRoom) slug
        runDB (insert newRoom) >>= \key -> sendResponseStatus status201 (toJSON (ChatRoomCreatedRp newRoom (fromSqlKey key) slug))
      Nothing  -> sendResponseStatus status401 ("UNAUTHORIZED" :: Text)

slugify :: Text -> ChatRoomSlug
slugify = ChatRoomSlug . toStrict . TLE.decodeUtf8 . makeSlug . TLE.encodeUtf8 . fromStrict 

{- | Replace using a regular expression. ByteString version. -}
regexReplace ::
    B.ByteString          -- ^ regular expression
    -> B.ByteString       -- ^ replacement text
    -> B.ByteString       -- ^ text to operate on
    -> B.ByteString
regexReplace regex replacement text = go text []
    where go str res =
              if B.null str
              then B.concat . reverse $ res
              else case (str =~~ regex) :: Maybe (B.ByteString, B.ByteString, B.ByteString) of
                     Nothing -> B.concat . reverse $ (str:res)
                     Just (bef, _ , aft) -> go aft (replacement:bef:res)

makeSlug :: B.ByteString -> B.ByteString
makeSlug = regexReplace "[ _]" "-"
          . regexReplace "[^a-z0-9_ ]+" ""
          . B.map toLower

getHomeR :: Handler Html
getHomeR = do
    chatRooms <- runDB (selectList [] [LimitTo 5]) :: Handler [Entity ChatRoom]
    defaultLayout $ do
        setTitle "Taplike / Home"
        $(widgetFile "homepage")

getLogOutR :: Handler Html
getLogOutR = clearSession >> getHomeR
