{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings    #-}

module Model.Instances where

import ClassyPrelude
import Types
import Control.Applicative (empty)
import Data.Aeson (object, (.=), (.:), ToJSON(toJSON), FromJSON(parseJSON), (.:?), withText, withObject, encode, decode, withScientific)
import Data.Aeson.Types (Parser, typeMismatch, Value(..))
import TextShow.TH (deriveTextShow)
import Network.WebSockets (WebSocketsData(fromLazyByteString, toLazyByteString))
import Taplike.TextShowOrphans ()
import Data.UUID.Aeson ()
import TextShow.Data.Time ()
import Data.Maybe (fromJust)
import TextShow (FromStringShow(FromStringShow), TextShow(showb), showt)
import Data.Scientific (toBoundedInteger)

instance FromJSON NewChannel where
  parseJSON (Object v) = NewChannel <$>
                         v .: "title" <*>
                         v .: "topic" <*>
                         v .: "color"
  parseJSON _          = empty

instance ToJSON NewChannel where
  toJSON (NewChannel title topic color) = object [ "title" .= title
                                                 , "topic" .= topic
                                                 , "color" .= color]

instance FromJSON NewMessageLike where
  parseJSON (Object v) = NewMessageLike <$>
                         v .: "message_id" <*>
                         v .: "channel"
  parseJSON _          = empty

instance ToJSON NewMessageLike where
  toJSON (NewMessageLike messageId channel) = object [ "message_id" .= messageId
                                                     , "channel"    .= channel
                                                     ]

deriving instance FromJSON ChannelTopic
deriving instance FromJSON ChannelTitle
deriving instance FromJSON ChannelColor
deriving instance FromJSON MessageId
deriving instance FromJSON UserId 
deriving instance FromJSON ChannelSlug
deriving instance FromJSON TwitterUserId
deriving instance FromJSON TwitterScreenName
deriving instance FromJSON ProfileImageUrl
deriving instance FromJSON NumberUsersPresent
deriving instance ToJSON ChannelTitle
deriving instance ToJSON ChannelTopic
deriving instance ToJSON ChannelColor
deriving instance ToJSON MessageId
deriving instance ToJSON ChannelSlug
deriving instance ToJSON UserId 
deriving instance ToJSON NumberUsersPresent
deriving instance ToJSON ProfileImageUrl
deriving instance ToJSON TwitterScreenName
deriving instance ToJSON TwitterUserId

instance ToJSON OkResponse where 
  toJSON _ = object [ "ok" .= True ]
  
instance WebSocketsData RtmEvent where
  fromLazyByteString = fromJust . decode
  toLazyByteString   = encode

instance ToJSON MessageText where
  toJSON (MessageText text) = String text

instance FromJSON MessageText where
  parseJSON (String s) = pure $ MessageText s
  parseJSON invalid    = typeMismatch "MessageText" invalid

instance ToJSON ChannelCreatedRp where
  toJSON (ChannelCreatedRp room roomId slug) = object ["chat_room" .= room, "id" .= roomId, "slug" .= slug]
  
instance ToJSON Channel where 
  toJSON (Channel creator created topic slug title numPresent color members) = object 
    [ "creator"           .= creator
    , "created"           .= created
    , "topic"             .= topic 
    , "slug"              .= slug 
    , "title"             .= title 
    , "num_users_present" .= numPresent
    , "color"             .= color
    , "members"           .= members
    ]

instance FromJSON Channel where
  parseJSON = withObject "channel object" $ \ o -> Channel
    <$> o .: "creator"  
    <*> o .: "created"  
    <*> o .: "topic"
    <*> o .: "slug"
    <*> o .: "title" 
    <*> o .: "num_users_present"
    <*> o .: "color" 
    <*> o .: "members" 
  
instance FromJSON TS where
  parseJSON = withText "timestamp" $ pure . TS

instance ToJSON TS where
  toJSON (TS t) = String t

-- instance FromJSON NumberUsersPresent where
--   parseJSON = withScientific "num_users_present" $ \ s ->
--     case toBoundedInteger s of
--       Just i64 -> pure (NumberUsersPresent i64)
--       Nothing  -> fail . unpack $ "out of bound Int64 " <> showt (FromStringShow s)
-- 
-- instance ToJSON NumberUsersPresent where
--   toJSON (NumberUsersPresent n) = Number n

deriveTextShow ''TS  
instance FromJSON (ID a) where
  parseJSON = withText "id" $ pure . ID

instance ToJSON (ID a) where
  toJSON = String . unID

deriveTextShow ''ID

deriving instance Eq RtmStartRp
deriving instance Eq Self
deriving instance Eq Message
deriving instance Eq RtmEvent
deriving instance Eq IncomingMessage
deriving instance Eq MessageText
deriving instance Eq Heartbeat
deriving instance Eq Ping
deriving instance Eq Pong
deriving instance Eq ReplyOk
deriving instance Eq ReplyNotOk
deriving instance Eq Presence
deriving instance Eq PresenceChange
deriving instance Eq MessageLikeAdded
deriving instance Eq Channel
deriving instance Eq User

deriveTextShow ''RtmStartRp
deriveTextShow ''Self
deriveTextShow ''Message
deriveTextShow ''RtmEvent
deriveTextShow ''IncomingMessage
deriveTextShow ''MessageText
deriveTextShow ''Heartbeat
deriveTextShow ''Ping
deriveTextShow ''Pong
deriveTextShow ''ReplyOk
deriveTextShow ''ReplyNotOk
deriveTextShow ''User
deriveTextShow ''UserId
deriveTextShow ''PresenceChange
deriveTextShow ''Presence
deriveTextShow ''MessageLikeAdded
deriveTextShow ''ChannelSlug
deriveTextShow ''TwitterUserId
deriveTextShow ''TwitterScreenName
deriveTextShow ''ProfileImageUrl
deriveTextShow ''MessageLike
deriveTextShow ''MessageId
deriveTextShow ''Channel
deriveTextShow ''NumberUsersPresent
deriveTextShow ''ChannelTopic
deriveTextShow ''ChannelTitle
deriveTextShow ''ChannelColor

instance FromJSON RtmStartRp where
  parseJSON = withObject "rtm.start reply" $ \ o -> RtmStartRp
    <$> o .:? "self"
    <*> o .: "users"
    <*> o .: "members"

instance ToJSON RtmStartRp where
  toJSON (RtmStartRp self users channels) = object
    [ "self"  .= self
    , "users" .= users
    , "channels" .= channels
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
    <*> o .: "likes"

instance ToJSON Message where
  toJSON (Message user text ts eventTs channel uuid msgLikes) =
    object ["type" .=  ("message" :: Text), "user" .= user, "text" .= text, "ts" .= ts
           , "event_ts" .= eventTs, "channel" .= channel, "uuid" .= uuid, "likes" .= msgLikes
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
  toJSON (User userId twitterUserId twitterScreenName profileImage presence) = object
    [ "profile_image_url" .= profileImage
    , "twitter_screen_name" .= twitterScreenName
    , "user_id" .= userId
    , "twitter_user_id" .= twitterUserId
    , "presence" .= presence
    ]

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User
    <$> o .: "user_id"
    <*> o .: "twitter_user_id" 
    <*> o .: "twitter_screen_name"
    <*> o .: "profile_image_url" 
    <*> o .: "presence" 

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

instance FromJSON MessageLike where
  parseJSON = withObject "Message Like" $ \o -> MessageLike
    <$> o .: "message_id"
    <*> o .: "user_id"
    <*> o .: "channel_slug"
    <*> o .: "timestamp"

instance ToJSON MessageLike where
  toJSON (MessageLike msgId userId channelSlug timestamp) = object
    [ "message_id"   .= msgId
    , "user_id"      .= userId
    , "channel_slug" .= channelSlug
    , "timestamp"    .= timestamp
    ]

instance ToJSON Presence where
  toJSON PresenceActive = String "active"
  toJSON PresenceAway   = String "away"
