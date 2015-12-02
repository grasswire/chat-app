module Taplike.Shared where

import           ClassyPrelude
import           Control.Lens (Getter, view, to)
import           Data.Aeson ((.:), (.:?), (.=), (.!=), Value(String), FromJSON(parseJSON), ToJSON(toJSON), object, withText, withObject, withScientific, withText, encode, decode)
import           Data.Aeson.Types (Parser, Value(..), typeMismatch)
import qualified Data.HashMap.Strict as HM
import           Data.Proxy (Proxy(Proxy))
import           Data.Scientific (toBoundedInteger)
import           TextShow (FromStringShow(FromStringShow), TextShow(showb), showt)
import           TextShow.TH (deriveTextShow)
import           Network.WebSockets hiding (Message, Response)
import           Taplike.TextShowOrphans ()
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Database.Persist.Class (Key(..))
import           Data.UUID
import           Data.Scientific (Scientific)
import           Model hiding (ChannelId, MessageText, Message, Heartbeat)
import           Data.UUID.Aeson()
import           TextShow.Data.Time()
import           Taplike.ChannelSlug
import           Data.Maybe (fromJust)

newtype ChannelId = ChannelId (Key Channel)
instance ToJSON ChannelId where
  toJSON (ChannelId key) = Number $ (fromInteger (fromIntegral $ fromSqlKey key :: Integer) :: Scientific)
instance FromJSON ChannelId where
  parseJSON = withScientific "channel_id" $ \ s ->
    case (toBoundedInteger s :: Maybe Int64) of
      Just i -> pure $ ChannelId (toSqlKey i)
      Nothing  -> fail . unpack $ "out of bound channel id " <> showt (FromStringShow s)
newtype MessageText = MessageText Text
instance ToJSON MessageText where
  toJSON (MessageText text) = String text
instance FromJSON MessageText where
  parseJSON (String s) = pure $ MessageText s
  parseJSON invalid    = typeMismatch "MessageText" invalid

data ChannelCreatedRp = ChannelCreatedRp
  { channelCreatedRpChannel :: Channel
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

newtype Time = Time { unTime :: Word32 } deriving (Eq, Ord)
instance FromJSON Time where
  parseJSON = withScientific "time" $ \ s ->
    case toBoundedInteger s of
      Just w32 -> pure (Time w32)
      Nothing  -> fail . unpack $ "out of bound unix time " <> showt (FromStringShow s)
deriveTextShow ''Time

newtype ID a = ID { unID :: Text } deriving (Eq, Ord)
instance FromJSON (ID a) where
  parseJSON = withText "id" $ pure . ID
instance ToJSON (ID a) where
  toJSON = String . unID
deriveTextShow ''ID

idedName :: Getter s Text -> Getter s (ID k) -> (s -> Text)
idedName name ident s = view name s ++ " <" ++ view (ident . to unID) s ++ ">"

data Response a = ResponseNotOk !Text | ResponseOk a

data RtmStartRequest = RtmStartRequest { rtmStartToken :: Text }

data RtmStartRp = RtmStartRp
  { _rtmStartUrl      :: Text
  , _rtmStartSelf     :: Maybe Self
  , _rtmStartUsers    :: [User]
  }

data Self = Self
  { _selfID               :: UserId
  , _selfName             :: Text
  , _selfProfileImageUrl  :: Text
  }

data Presence = PresenceActive | PresenceAway

data Profile = Profile
  { _profileFirstName :: Maybe Text
  , _profileLastName  :: Maybe Text
  , _profileRealName  :: Maybe Text
  , _profileEmail     :: Maybe Text
  , _profileSkype     :: Maybe Text
  , _profilePhone     :: Maybe Text }

data Bot = Bot
  { _botID    :: ID Bot
  , _botName  :: Text
  , _botIcons :: HM.HashMap Text Text }

data Chat

data Message = Message
  { _messageUser         :: UserId
  , _messageText         :: MessageText
  , _messageTS           :: TS
  , _messageEventTS      :: Maybe TS
  , _messageChannel      :: ChannelId
  }

data IncomingMessage = IncomingMessage
 { incomingMessageUUID        :: UUID
 , incomingMessageTS          :: UTCTime
 , incomingMessageChannelId   :: ChannelId
 , incomingMessageMessageText :: MessageText
 }

testMessage :: Int64 -> UserId -> Text -> Message
testMessage chat from text = Message
  { _messageUser         = from
  , _messageText         = MessageText text
  , _messageTS           = TS "0"
  , _messageEventTS      = Nothing
  , _messageChannel      = ChannelId $ ChannelKey 1
 }

data TapLikeTracked a = TapLikeTracked
  { _trackedValue   :: a
  , _trackedCreator :: ID User
  , _trackedLastSet :: Time
  }

data Heartbeat = Heartbeat
  { heartBeatUser :: UserId
  , heartBeatChannel :: ChannelId
  }

data RtmEvent
  = RtmHello
  | RtmReplyOk Word64 (Maybe TS) (Maybe Text)
  | RtmReplyNotOk Word64 Int32 Text
  | RtmMessage Message
  | RtmUserTyping UserTyping
  | RtmSendMessage IncomingMessage
  | RtmHeartbeat Heartbeat

instance WebSocketsData RtmEvent where
  fromLazyByteString = fromJust . decode
  toLazyByteString   = encode

data UserTyping = UserTyping
  { _userTypingUser    :: ID User
  , _userTypingChannel :: ID Chat }

class TapLikeTyped a where
  isTypedID :: Proxy a -> ID b -> Bool
instance TapLikeTyped Channel where
  isTypedID _ = isPrefixOf "C" . unID
instance TapLikeTyped Chat where
   isTypedID _ i
    =  isTypedID (Proxy :: Proxy Channel) i

instance TapLikeTyped User where
  isTypedID _ = isPrefixOf "U" . unID

asTypedID :: forall a b. TapLikeTyped b => ID a -> Maybe (ID b)
asTypedID i =
  if isTypedID (Proxy :: Proxy b) i
    then Just (ID . unID $ i)
    else Nothing

asChannelID :: ID Chat -> Maybe (ID Channel)
asChannelID = asTypedID

deriving instance Eq RtmStartRequest
deriving instance Eq RtmStartRp
deriving instance Eq Self
deriving instance Eq Message
deriving instance Eq a => Eq (TapLikeTracked a)
deriving instance Eq RtmEvent
deriving instance Eq UserTyping
deriving instance Eq IncomingMessage
deriving instance Eq MessageText
deriving instance Eq ChannelId
deriving instance Eq Heartbeat

instance TextShow Chat where
  showb _ = "Chat"

deriveTextShow ''RtmStartRequest
deriveTextShow ''RtmStartRp
deriveTextShow ''Self
deriveTextShow ''Message
deriveTextShow ''TapLikeTracked
deriveTextShow ''RtmEvent
deriveTextShow ''UserTyping
deriveTextShow ''IncomingMessage
deriveTextShow ''MessageText
deriveTextShow ''ChannelId
deriveTextShow ''Heartbeat


instance ToJSON RtmStartRequest where
  toJSON (RtmStartRequest { .. }) = object
    [ ("token", toJSON rtmStartToken) ]

instance FromJSON a => FromJSON (Response a) where
  parseJSON = withObject "TapLike reply" $ \ o ->
    o .: "ok" >>= \ case
    True -> ResponseOk <$> parseJSON (Object o)
    False -> ResponseNotOk <$> o .:? "error" .!= "unknown error"

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
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "profile_image_url"

instance ToJSON Self where
  toJSON (Self sId name profileImageUrl) = object
    [ "id"                .= sId
    , "name"              .= name
    , "profile_image_url" .= profileImageUrl
    ]

instance FromJSON a => FromJSON (TapLikeTracked a) where
  parseJSON = withObject "tracked value object" $ \ o -> TapLikeTracked
    <$> o .: "value"
    <*> o .: "creator"
    <*> o .: "last_set"

instance FromJSON Message where
  parseJSON = withObject "message object" $ \ o -> Message
    <$> o .: "user"
    <*> o .: "text"
    <*> o .: "ts"
    <*> o .:? "event_ts"
    <*> o .: "channel"

instance ToJSON Message where
  toJSON (Message user text ts eventTs channel) =
    object ["type" .=  ("message" :: Text), "user" .= user, "text" .= text, "ts" .= ts
           , "event_ts" .= eventTs, "channel" .= channel
           ]

instance FromJSON RtmEvent where
  parseJSON v =
    let recur :: FromJSON a => Parser a
        recur = parseJSON v
    in flip (withObject "event object") v $ \ o ->
        o .:? "reply_to" >>= \ case
          Just seqnum ->
            o .: "ok" >>= \ case
              True  -> RtmReplyOk seqnum <$> o .:? "ts" <*> o .:? "text"
              False -> o .: "error" >>= (withObject "RTM error" $ \ o2 -> RtmReplyNotOk seqnum <$> o2 .: "code" <*> o2 .: "msg")
          Nothing ->
            o .: "type" >>= pure . asText >>= \ case
              "hello"                   -> pure RtmHello
              "message"                 -> RtmMessage <$> recur
              "user_typing"             -> RtmUserTyping <$> recur
              "incoming_message"        -> RtmSendMessage <$> recur
              "heart_beat"              -> RtmHeartbeat <$> recur
              other                     -> fail . unpack $ "unknown RTM event type " <> other

instance ToJSON RtmEvent where
  toJSON event = case event of
                  RtmSendMessage msg -> toJSON msg
                  RtmMessage message -> toJSON message
                  RtmHeartbeat beat ->  toJSON beat
                  RtmHello           -> object ["type" .= ("hello" :: Text)]

instance FromJSON UserTyping where
  parseJSON = withObject "user typing event" $ \ o -> UserTyping
    <$> o .: "user"
    <*> o .: "channel"

instance ToJSON IncomingMessage where
  toJSON (IncomingMessage uuid ts channelId msgText) = object
    [ "type"         .= ("incoming_message" :: Text)
    , "uuid"         .= uuid
    , "timestamp"    .= ts
    , "channel_id"   .= channelId
    , "message_text" .= msgText
    ]

instance FromJSON IncomingMessage where
  parseJSON  = withObject "incoming message" $ \ o ->  IncomingMessage
      <$> o .: "uuid"
      <*> o .: "timestamp"
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
    , "channel"    .= channel
    ]
