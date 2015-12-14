module Taplike.Shared where

import           ClassyPrelude
import           Control.Lens (Getter, view, to)
import           Data.Aeson ((.:), (.:?), (.=), (.!=), Value(String), FromJSON(parseJSON), ToJSON(toJSON), object, withText, withObject, withScientific, withText, encode, decode)
import           Data.Aeson.Types (Parser, Value(..), typeMismatch)
import qualified Data.HashMap.Strict as HM
import           Data.Proxy (Proxy(Proxy))
import           Data.Scientific (toBoundedInteger, Scientific)
import           TextShow (FromStringShow(FromStringShow), TextShow(showb), showt)
import           TextShow.TH (deriveTextShow)
import           Network.WebSockets (WebSocketsData(fromLazyByteString, toLazyByteString))
import           Taplike.TextShowOrphans ()
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Database.Persist.Class (Key(..))
import           Data.UUID
import           Model hiding (ChannelId, MessageText, Message, Heartbeat, User)
import           Data.UUID.Aeson()
import           TextShow.Data.Time()
import           Taplike.Schema
import           Data.Maybe (fromJust)
import qualified Model as Model
import Database.Persist.Types (Entity(..))

newtype ChannelId = ChannelId (Key Channel)
instance ToJSON ChannelId where
  toJSON (ChannelId key) = Number (fromInteger (fromIntegral $ fromSqlKey key :: Integer) :: Scientific)
instance FromJSON ChannelId where
  parseJSON = withScientific "channel_id" $ \ s ->
    case (toBoundedInteger s :: Maybe Int64) of
      Just i -> pure $ ChannelId (toSqlKey i)
      Nothing  -> fail . unpack $ "out of bound channel id " <> showt (FromStringShow s)
newtype MessageText = MessageText { unMessageText :: Text}
instance ToJSON MessageText where
  toJSON (MessageText text) = String text
instance FromJSON MessageText where
  parseJSON (String s) = pure $ MessageText s
  parseJSON invalid    = typeMismatch "MessageText" invalid

data ChannelCreatedRp = ChannelCreatedRp
  { channelCreatedRpChannel  :: Channel
  , channelCreatedRpId       :: Int64
  , channelSlug              :: ChannelSlug
  }

instance ToJSON ChannelCreatedRp where
  toJSON (ChannelCreatedRp room roomId slug) = object ["chat_room" .= room, "id" .= roomId, "slug" .= slug]

newtype TS = TS { unTS :: Text } deriving (Eq, Ord)
instance FromJSON TS where
  parseJSON = withText "timestamp" $ pure . TS
instance ToJSON TS where
  toJSON (TS t) = String t
deriveTextShow ''TS

newtype ID a = ID { unID :: Text } deriving (Eq, Ord)
instance FromJSON (ID a) where
  parseJSON = withText "id" $ pure . ID
instance ToJSON (ID a) where
  toJSON = String . unID
deriveTextShow ''ID

idedName :: Getter s Text -> Getter s (ID k) -> (s -> Text)
idedName name ident s = view name s ++ " <" ++ view (ident . to unID) s ++ ">"

data RtmStartRequest = RtmStartRequest { rtmStartToken :: Text }

data User = User
  { userProfileImageUrl :: Text
  , userTwitterScreenName :: Text
  , userUserId :: UserId
  }

userFromEntity :: Entity Model.User -> User
userFromEntity userEntity = User (Model.userProfileImageUrl userVal) (Model.userTwitterScreenName userVal) key
  where userVal = entityVal userEntity
        key     = entityKey userEntity

data RtmStartRp = RtmStartRp
  { rtmStartUrl      :: Text
  , rtmStartSelf     :: Maybe Self
  , rtmStartUsers    :: [User]
  }

data Self = Self
  { selfID               :: UserId
  , selfName             :: Text
  , selfProfileImageUrl  :: Text
  }

data Presence = PresenceActive | PresenceAway

data Profile = Profile
  { profileFirstName :: Maybe Text
  , profileLastName  :: Maybe Text
  , profileRealName  :: Maybe Text
  , profileEmail     :: Maybe Text
  , profileSkype     :: Maybe Text
  , profilePhone     :: Maybe Text }

data Message = Message
  { messageUser         :: UserId
  , messageText         :: MessageText
  , messageTS           :: UTCTime
  , messageEventTS      :: Maybe UTCTime
  , messageChannel      :: Text
  , messageUUID         :: UUID
  }

data IncomingMessage = IncomingMessage
 { incomingMessageUUID        :: UUID
 , incomingMessageTS          :: UTCTime
 , incomingMessageChannelId   :: Text
 , incomingMessageMessageText :: MessageText
 }

data Heartbeat = Heartbeat
  { heartBeatUser :: UserId
  , heartBeatChannel :: ChannelId
  }

data RtmEvent
  = RtmHello
  | RtmReplyOk ReplyOk
  | RtmReplyNotOk ReplyNotOk
  | RtmMessage Message
  | RtmSendMessage IncomingMessage
  | RtmHeartbeat Heartbeat
  | RtmPing Ping
  | RtmPong Pong
  | RtmPresenceChange PresenceChange
  | RtmMessageLikeAdded MessageLikeAdded 
  
data MessageLikeAdded = MessageLikeAdded 
  { messageLikeAddedUser      :: UserId
  , messageLikeAddedMessageId :: UUID  
  }

data ReplyOk = ReplyOk
  { replyOkReplyTo :: UUID
  , replyOkTS      :: Maybe UTCTime
  , replyOkText    :: Maybe Text
  }

data ReplyNotOk = ReplyNotOk
  { replyNotOkReplyTo :: UUID
  , replyNotOkCode    :: Int32
  , replyNotOkText    :: Text
  }

data Ping = Ping { pingId :: Int32 }
data Pong = Pong { pongReplyTo :: Int32 }

data PresenceChange = PresenceChange
  { presenceChangeUser     :: User
  , presenceChangePresence :: Presence
  }

instance WebSocketsData RtmEvent where
  fromLazyByteString = fromJust . decode
  toLazyByteString   = encode

deriving instance Eq RtmStartRequest
deriving instance Eq RtmStartRp
deriving instance Eq Self
deriving instance Eq Message
deriving instance Eq RtmEvent
deriving instance Eq IncomingMessage
deriving instance Eq MessageText
deriving instance Eq ChannelId
deriving instance Eq Heartbeat
deriving instance Eq Ping
deriving instance Eq Pong
deriving instance Eq ReplyOk
deriving instance Eq ReplyNotOk
deriving instance Eq User
deriving instance Eq Presence
deriving instance Eq PresenceChange
deriving instance Eq MessageLikeAdded

deriveTextShow ''RtmStartRequest
deriveTextShow ''RtmStartRp
deriveTextShow ''Self
deriveTextShow ''Message
deriveTextShow ''RtmEvent
deriveTextShow ''IncomingMessage
deriveTextShow ''MessageText
deriveTextShow ''ChannelId
deriveTextShow ''Heartbeat
deriveTextShow ''Ping
deriveTextShow ''Pong
deriveTextShow ''ReplyOk
deriveTextShow ''ReplyNotOk
deriveTextShow ''User
deriveTextShow ''PresenceChange
deriveTextShow ''Presence
deriveTextShow ''MessageLikeAdded

instance ToJSON RtmStartRequest where
  toJSON (RtmStartRequest { .. }) = object
    [ ("token", toJSON rtmStartToken) ]

instance FromJSON RtmStartRp where
  parseJSON = withObject "rtm.start reply" $ \ o -> RtmStartRp
    <$> o .: "url"
    <*> o .:? "self"
    <*> o .: "users"

instance ToJSON RtmStartRp where
  toJSON (RtmStartRp url self users) = object
    [ "url"   .= url
    , "self"  .= self
    , "users" .= users
    ]

instance FromJSON Self where
  parseJSON = withObject "self object" $ \ o -> Self
    <$> o .: "user_id"
    <*> o .: "twitter_screen_name"
    <*> o .: "profile_image_url"

instance ToJSON Self where
  toJSON (Self sId name profileImageUrl) = object
    [ "user_id"             .= sId
    , "twitter_screen_name" .= name
    , "profile_image_url"   .= profileImageUrl
    ]

instance FromJSON Message where
  parseJSON = withObject "message object" $ \ o -> Message
    <$> o .: "user"
    <*> o .: "text"
    <*> o .: "ts"
    <*> o .:? "event_ts"
    <*> o .: "channel"
    <*> o .: "uuid"

instance ToJSON Message where
  toJSON (Message user text ts eventTs channel uuid) =
    object ["type" .=  ("message" :: Text), "user" .= user, "text" .= text, "ts" .= ts
           , "event_ts" .= eventTs, "channel" .= channel, "uuid" .= uuid
           ]

instance FromJSON Presence where
 parseJSON = withText "presence value" $ \ case
   "active" -> pure PresenceActive
   "away"   -> pure PresenceAway
   other    -> fail . unpack $ "unknown presence value " <> other

instance FromJSON PresenceChange where
 parseJSON = withObject "presence change event" $ \ o -> PresenceChange
   <$> o .: "user"
   <*> o .: "presence"

instance FromJSON RtmEvent where
  parseJSON v =
    let recur :: FromJSON a => Parser a
        recur = parseJSON v
    in flip (withObject "event object") v $ \ o ->
            o .: "type" >>= pure . asText >>= \ case
              "hello"                   -> pure RtmHello
              "ping"                    -> RtmPing <$> recur
              "pong"                    -> RtmPong <$> recur
              "message"                 -> RtmMessage <$> recur
              "incoming_message"        -> RtmSendMessage <$> recur
              "heart_beat"              -> RtmHeartbeat <$> recur
              "ok"                      -> RtmReplyOk <$> recur
              "not_ok"                  -> RtmReplyNotOk <$> recur
              "presence_change"         -> RtmPresenceChange <$> recur
              "message_like_added"      -> RtmMessageLikeAdded <$> recur
              other                     -> fail . unpack $ "unknown RTM event type " <> other

instance ToJSON RtmEvent where
  toJSON event = case event of
                  RtmSendMessage msg            -> toJSON msg
                  RtmMessage message            -> toJSON message
                  RtmHeartbeat beat             -> toJSON beat
                  RtmHello                      -> object ["type" .= ("hello" :: Text)]
                  RtmPing ping                  -> toJSON ping
                  RtmPong pong                  -> toJSON pong
                  RtmReplyOk ok                 -> toJSON ok
                  RtmReplyNotOk notok           -> toJSON notok
                  RtmMessageLikeAdded likeAdded -> toJSON likeAdded
                  RtmPresenceChange change      -> toJSON change

instance ToJSON IncomingMessage where
  toJSON (IncomingMessage uuid ts channelId msgText) = object
    [ "type"         .= ("incoming_message" :: Text)
    , "uuid"         .= uuid
    , "ts"           .= ts
    , "channel_id"   .= channelId
    , "message_text" .= msgText
    ]

instance FromJSON IncomingMessage where
  parseJSON  = withObject "incoming message" $ \ o ->  IncomingMessage
      <$> o .: "uuid"
      <*> o .: "ts"
      <*> o .: "channel_id"
      <*> o .: "message_text"

instance FromJSON Heartbeat where
  parseJSON = withObject "presence heartbeat" $ \o -> Heartbeat
    <$> o .: "user"
    <*> o .: "channel"

instance ToJSON Heartbeat where
  toJSON (Heartbeat user channel) = object
    [ "type"         .= ("heart_beat" :: Text)
    , "user"         .= user
    , "channel"      .= channel
    ]
instance FromJSON Ping where
  parseJSON = withObject "ping" $ \o -> Ping
    <$> o .: "id"

instance ToJSON Ping where
  toJSON (Ping pId) = object
    [ "type"         .= ("ping" :: Text)
    , "id"           .= pId
    ]
instance FromJSON Pong where
  parseJSON = withObject "pong" $ \o -> Pong
    <$> o .: "reply_to"

instance ToJSON Pong where
  toJSON (Pong replyTo) = object
    [ "type"         .= ("pong" :: Text)
    , "reply_to"     .= replyTo
    ]

instance ToJSON ReplyOk where
  toJSON (ReplyOk replyTo ts text) = object
    [ "type"  .= ("ok" :: Text)
    , "reply_to" .= replyTo
    , "ts" .= ts
    , "text" .= text
    ]

instance ToJSON ReplyNotOk where
  toJSON (ReplyNotOk replyTo code msg) = object
    [ "type"  .= ("not_ok" :: Text)
    , "reply_to" .= replyTo
    , "code" .= code
    , "msg" .= msg
    ]

instance FromJSON ReplyOk where
  parseJSON = withObject "reply ok" $ \o -> ReplyOk
    <$> o .: "reply_to"
    <*> o .: "ts"
    <*> o .: "text"

instance FromJSON ReplyNotOk where
  parseJSON = withObject "reply not ok" $ \o -> ReplyNotOk
    <$> o .: "reply_to"
    <*> o .: "code"
    <*> o .: "msg"

instance ToJSON User where
  toJSON (User image screenName userId) = object
    [ "profile_image_url" .= image
    , "twitter_screen_name" .= screenName
    , "user_id" .= userId
    ]

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User
    <$> o .: "profile_image_url"
    <*> o .: "twitter_screen_name"
    <*> o .: "user_id"

instance ToJSON PresenceChange where
  toJSON (PresenceChange user presence) = object
    [ "type"  .= ("presence_change" :: Text)
    , "user" .= user
    , "presence" .= presence
    ]

instance FromJSON MessageLikeAdded where
  parseJSON = withObject "Message Like Added" $ \o -> MessageLikeAdded
    <$> o .: "user_id"
    <*> o .: "message_id"

instance ToJSON MessageLikeAdded where
  toJSON (MessageLikeAdded user message) = object
    [ "type"       .= ("message_like_added" :: Text)
    , "user_id"    .= user
    , "message_id" .= message
    ]

instance ToJSON Presence where
  toJSON PresenceActive = String "active"
  toJSON PresenceAway   = String "away"
