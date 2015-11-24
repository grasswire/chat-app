module Taplike.Shared where

import           ClassyPrelude
import           Control.Lens (Getter, view, to)
import           Data.Aeson ((.:), (.:?), (.=), (.!=), Value(Object, String), Object, FromJSON(parseJSON), ToJSON(toJSON), object, withText, withObject, withScientific, withText)
import           Data.Aeson.Types (Parser, Value(..), typeMismatch)
import qualified Data.HashMap.Strict as HM
import           Data.Proxy (Proxy(Proxy))
import           Data.Scientific (toBoundedInteger)
import           TextShow (FromStringShow(FromStringShow), TextShow(showb), showt)
import           TextShow.TH (deriveTextShow)

import           Taplike.TextShowOrphans ()
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Persist.Class (Key(..))
import Data.UUID.V4 as UUIDV4
import Data.UUID (UUID(..))
import Data.Scientific (Scientific)
import Model
import Data.UUID.Aeson()
import TextShow.Data.Time()

instance TextShow (Key ChatRoom) where
  showb = showb . fromSqlKey

newtype ChannelId = ChannelId (Key ChatRoom)
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

data ChatRoomCreatedRp = ChatRoomCreatedRp
  { chatRoomCreatedRpChatRoom :: ChatRoom
  , chatRoomCreatedRpId       :: Int64
  }

instance ToJSON ChatRoomCreatedRp where
  toJSON (ChatRoomCreatedRp room roomId) = object ["chat_room" .= room, "id" .= roomId]

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
  { _selfID               :: Int64
  , _selfName             :: Text
  , _selfProfileImageUrl  :: Text
  }

data Presence = PresenceActive | PresenceAway

data Team = Team
  { _teamID                :: ID Team
  , _teamName              :: Text
  , _teamEmailDomain       :: Maybe Text
  , _teamDomain            :: Text
  , _teamMsgEditWindowMins :: Maybe Int
  , _teamOverStorageLimit  :: Bool
  , _teamPrefs             :: Object }

data Profile = Profile
  { _profileFirstName :: Maybe Text
  , _profileLastName  :: Maybe Text
  , _profileRealName  :: Maybe Text
  , _profileEmail     :: Maybe Text
  , _profileSkype     :: Maybe Text
  , _profilePhone     :: Maybe Text }

data Channel = Channel
  { _channelID          :: ID Channel
  , _channelName        :: Text
  , _channelCreated     :: Time
  , _channelCreator     :: ID User
  , _channelIsArchived  :: Bool
  , _channelIsGeneral   :: Bool
  , _channelMembers     :: [ID User]
  , _channelTopic       :: Maybe (TapLikeTracked Text)
  , _channelPurpose     :: Maybe (TapLikeTracked Text)
  , _channelIsMember    :: Bool
  , _channelLastRead    :: Maybe TS
  , _channelLatest      :: Maybe Message
  , _channelUnreadCount :: Maybe Int }

data Group = Group
  { _groupID          :: ID Group
  , _groupName        :: Text
  , _groupCreated     :: Time
  , _groupCreator     :: ID User
  , _groupIsArchived  :: Bool
  , _groupMembers     :: [ID User]
  , _groupTopic       :: Maybe (TapLikeTracked Text)
  , _groupPurpose     :: Maybe (TapLikeTracked Text)
  , _groupIsOpen      :: Bool
  , _groupLastRead    :: Maybe TS
  , _groupLatest      :: Maybe Message
  , _groupUnreadCount :: Maybe Int }

data Bot = Bot
  { _botID    :: ID Bot
  , _botName  :: Text
  , _botIcons :: HM.HashMap Text Text }

data Chat

data Message = Message
  { _messageUser         :: Int64
  , _messageSubtype      :: Maybe MessageSubtype
  , _messageText         :: MessageText
  , _messageTS           :: TS
  , _messageEdited       :: Maybe MessageEdited
  , _messageDeletedTS    :: Maybe TS
  , _messageEventTS      :: Maybe TS
  , _messageHidden       :: Bool
  , _messageIsStarred    :: Maybe Bool
  , _messagePinnedTo     :: [ID Channel]
  }

data IncomingMessage = IncomingMessage
 { incomingMessageUUID        :: UUID
 , incomingMessageTS          :: UTCTime
 , incomingMessageChannelId   :: ChannelId
 , incomingMessageMessageText :: MessageText
 }

testMessage :: Int64 -> Int64 -> Text -> Message
testMessage chat from text = Message
  { _messageUser         = from
  , _messageSubtype      = Nothing
  , _messageText         = MessageText text
  , _messageTS           = TS "0"
  , _messageEdited       = Nothing
  , _messageDeletedTS    = Nothing
  , _messageEventTS      = Nothing
  , _messageHidden       = False
  , _messageIsStarred    = Nothing
  , _messagePinnedTo     = []
 }

data MessageSubtype
  = BotMS | MeMS | ChangedMS | DeletedMS
  | ChannelTopicMS | ChannelPurposeMS | ChannelNameMS | ChannelArchiveMS | ChannelUnarchiveMS

data MessageEdited = MessageEdited
  { _messageEditedUser :: Int64
  , _messageEditedTS   :: TS }

data MessageReaction = MessageReaction
  { _messageReactionName :: Text
  , _messageReactionCount :: Int
  , _messageReactionUsers :: [ID User] }

data TapLikeTracked a = TapLikeTracked
  { _trackedValue   :: a
  , _trackedCreator :: ID User
  , _trackedLastSet :: Time
  }

data RtmEvent
  = RtmHello
  | RtmReplyOk Word64 (Maybe TS) (Maybe Text)
  | RtmReplyNotOk Word64 Int32 Text
  | RtmMessage Message
  | RtmChannelMarked (ChatMarked Channel)
  | RtmChannelCreated Channel
  | RtmChannelDeleted (ID Channel)
  | RtmChannelRenamed (ChatRenamed Channel)
  | RtmChannelArchive (ChatUser Channel)
  | RtmChannelUnarchive (ChatUser Channel)
  | RtmChannelHistoryChanged (ChatHistoryChanged Channel)
  | RtmPresenceChange PresenceChange
  | RtmManualPresenceChange Presence
  | RtmPrefChange PrefChange
  | RtmUserChange User
  | RtmUserTyping UserTyping
  | RtmStarAdded Star
  | RtmStarRemoved Star
  | RtmEmojiChanged TS
  | RtmCommandsChanged TS
  | RtmTeamPrefChange PrefChange
  | RtmBotAdded Bot
  | RtmBotChanged Bot
  | RtmAccountsChanged
  | RtmSendMessage IncomingMessage

data ChatMarked a = ChatMarked
  { _chatMarkedChannel :: ID a
  , _chatMarkedTS      :: TS }

data ChatUser a = ChatUser
  { _chatUserUser      :: ID User
  , _chatUserChannelID :: ID a }

data ChatRenamed a = ChatRenamed
  { _chatRenamedChannelID :: ID a
  , _chatRenamedName      :: Text }

data ChatHistoryChanged a = ChatHistoryChanged
  { _chatHistoryChangedLatest  :: Text
  , _chatHistoryChangedTS      :: TS
  , _chatHistoryChangedEventTS :: TS }

data PresenceChange = PresenceChange
  { _presenceChangeUser     :: ID User
  , _presenceChangePresence :: Presence }

data PrefChange = PrefChange
  { _prefChangeName  :: Text
  , _prefChangeValue :: Value }

data UserTyping = UserTyping
  { _userTypingUser    :: ID User
  , _userTypingChannel :: ID Chat }

data Star = Star
  { _starUser    :: Text
  , _starItem    :: StarItem
  , _starEventTS :: TS }

data StarItem
  = StarItemMessage Message
  | StarItemChannel (ID Channel)

data TeamDomainChange = TeamDomainChange
  { _teamDomainChangeUrl    :: Text
  , _teamDomainChangeDomain :: Text }

data EmailDomainChanged = EmailDomainChanged
  { _emailDomainChangedEmailDomain :: Text
  , _emailDomainChangedEventTS     :: TS }

class TapLikeTyped a where
  isTypedID :: Proxy a -> ID b -> Bool
instance TapLikeTyped Channel where
  isTypedID _ = isPrefixOf "C" . unID
instance TapLikeTyped Group where
  isTypedID _ = isPrefixOf "G" . unID
instance TapLikeTyped Chat where
   isTypedID _ i
    =  isTypedID (Proxy :: Proxy Channel) i
    || isTypedID (Proxy :: Proxy Group) i

instance TapLikeTyped User where
  isTypedID _ = isPrefixOf "U" . unID

asTypedID :: forall a b. TapLikeTyped b => ID a -> Maybe (ID b)
asTypedID i =
  if isTypedID (Proxy :: Proxy b) i
    then Just (ID . unID $ i)
    else Nothing

asChannelID :: ID Chat -> Maybe (ID Channel)
asChannelID = asTypedID
asGroupID :: ID Chat -> Maybe (ID Group)
asGroupID = asTypedID


deriving instance Eq RtmStartRequest
deriving instance Eq RtmStartRp
deriving instance Eq Self
deriving instance Eq Team
deriving instance Eq User
deriving instance Eq Profile
deriving instance Eq Channel
deriving instance Eq Group
deriving instance Eq Bot
deriving instance Eq MessageSubtype
deriving instance Eq MessageReaction
deriving instance Eq Message
deriving instance Eq MessageEdited
deriving instance Eq a => Eq (TapLikeTracked a)
deriving instance Eq RtmEvent
deriving instance Eq a => Eq (ChatMarked a)
deriving instance Eq a => Eq (ChatUser a)
deriving instance Eq a => Eq (ChatRenamed a)
deriving instance Eq a => Eq (ChatHistoryChanged a)
deriving instance Eq Presence
deriving instance Eq PresenceChange
deriving instance Eq UserTyping
deriving instance Eq PrefChange
deriving instance Eq Star
deriving instance Eq StarItem
deriving instance Eq TeamDomainChange
deriving instance Eq EmailDomainChanged
deriving instance Eq IncomingMessage
deriving instance Eq MessageText
deriving instance Eq ChannelId

instance TextShow Chat where
  showb _ = "Chat"

deriveTextShow ''RtmStartRequest
deriveTextShow ''RtmStartRp
deriveTextShow ''Self
deriveTextShow ''Presence
deriveTextShow ''Team
deriveTextShow ''User
deriveTextShow ''Profile
deriveTextShow ''Channel
deriveTextShow ''Group
deriveTextShow ''Bot
deriveTextShow ''Message
deriveTextShow ''MessageSubtype
deriveTextShow ''MessageEdited
deriveTextShow ''MessageReaction
deriveTextShow ''TapLikeTracked
deriveTextShow ''RtmEvent
deriveTextShow ''ChatMarked
deriveTextShow ''ChatUser
deriveTextShow ''ChatRenamed
deriveTextShow ''ChatHistoryChanged
deriveTextShow ''PresenceChange
deriveTextShow ''UserTyping
deriveTextShow ''PrefChange
deriveTextShow ''Star
deriveTextShow ''StarItem
deriveTextShow ''TeamDomainChange
deriveTextShow ''EmailDomainChanged
deriveTextShow ''IncomingMessage
deriveTextShow ''UUID
deriveTextShow ''MessageText
deriveTextShow ''ChannelId


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

instance FromJSON Presence where
  parseJSON = withText "presence value" $ \ case
    "active" -> pure PresenceActive
    "away"   -> pure PresenceAway
    other    -> fail . unpack $ "unknown presence value " <> other

instance FromJSON Team where
  parseJSON = withObject "team object" $ \ o -> Team
    <$> o .: "id"
    <*> o .: "name"
    <*> (o .:? "email_domain" >>= \ case
          Just "" -> pure Nothing
          Just s  -> pure $ Just s
          Nothing -> pure Nothing)
    <*> o .: "domain"
    <*> (o .:? "msg_edit_window_mins" >>= \ case
          Just (-1) -> pure Nothing
          Just i    -> pure $ Just i
          Nothing   -> pure Nothing)
    <*> o .: "over_storage_limit"
    <*> o .: "prefs"

instance FromJSON Profile where
  parseJSON = withObject "user profile object" $ \ o -> Profile
    <$> o .:? "first_name"
    <*> o .:? "last_name"
    <*> o .:? "real_name"
    <*> o .:? "email"
    <*> o .:? "skype"
    <*> o .:? "phone"

instance FromJSON Channel where
  parseJSON = withObject "channel object" $ \ o -> Channel
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "created"
    <*> o .: "creator"
    <*> o .: "is_archived"
    <*> o .:? "is_general" .!= False
    <*> o .:? "members" .!= []
    <*> o .:? "topic"
    <*> o .:? "purpose"
    <*> o .:? "is_member" .!= False
    <*> o .:? "last_read"
    <*> o .:? "latest"
    <*> o .:? "unread_count"

instance FromJSON Group where
  parseJSON = withObject "group object" $ \ o -> Group
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "created"
    <*> o .: "creator"
    <*> o .: "is_archived"
    <*> o .:? "members" .!= []
    <*> o .:? "topic"
    <*> o .:? "purpose"
    <*> o .:? "is_open" .!= False
    <*> o .:? "last_read"
    <*> o .:? "latest"
    <*> o .:? "unread_count"



instance FromJSON Bot where
  parseJSON = withObject "bot object" $ \ o -> Bot
    <$> o .: "id"
    <*> o .: "name"
    <*> o .:? "icons" .!= HM.empty

instance FromJSON a => FromJSON (TapLikeTracked a) where
  parseJSON = withObject "tracked value object" $ \ o -> TapLikeTracked
    <$> o .: "value"
    <*> o .: "creator"
    <*> o .: "last_set"

instance FromJSON Message where
  parseJSON = withObject "message object" $ \ o -> Message
    <$> o .: "user"
    <*> o .:? "subtype"
    <*> o .: "text"
    <*> o .: "ts"
    <*> o .:? "edited"
    <*> o .:? "deleted_ts"
    <*> o .:? "event_ts"
    <*> o .:? "hidden" .!= False
    <*> o .:? "is_starred"
    <*> o .:? "pinned_to" .!= []

instance ToJSON Message where
  toJSON (Message user subtype text ts edited deletedTs eventTs hidden isStarred pinnedTo) =
    object ["type" .=  ("message" :: Text), "user" .= user, "subtype" .= parseSubType subtype, "text" .= text, "ts" .= ts
           , "edited" .= edited, "deleted_ts" .= deletedTs, "event_ts" .= eventTs, "hidden" .= hidden
           , "is_starred" .= isStarred, "pinned_to" .= pinnedTo]
    where parseSubType st = case st of
                              Just BotMS              -> String ("bot_message" :: Text)
                              Just MeMS               -> String ("me_message" :: Text)
                              Just ChangedMS          -> String ("message_changed" :: Text)
                              Just DeletedMS          -> String ("message_deleted" :: Text)
                              Just ChannelTopicMS     -> String ("channel_topic" :: Text)
                              Just ChannelPurposeMS   -> String ("channel_purpose" :: Text)
                              Just ChannelNameMS      -> String ("channel_name" :: Text)
                              Just ChannelArchiveMS   -> String ("channel_archive" :: Text)
                              Just ChannelUnarchiveMS -> String ("channel_unarchive" :: Text)
                              Nothing                 -> Null


instance FromJSON MessageSubtype where
  parseJSON = withText "message subtype" $ \ case
    "bot_message"       -> pure BotMS
    "me_message"        -> pure MeMS
    "message_changed"   -> pure ChangedMS
    "message_deleted"   -> pure DeletedMS
    "channel_topic"     -> pure ChannelTopicMS
    "channel_purpose"   -> pure ChannelPurposeMS
    "channel_name"      -> pure ChannelNameMS
    "channel_archive"   -> pure ChannelArchiveMS
    "channel_unarchive" -> pure ChannelUnarchiveMS
    other               -> fail . unpack $ "unknown message subtype " <> other

instance FromJSON MessageEdited where
  parseJSON = withObject "message edited object" $ \ o -> MessageEdited
    <$> o .: "user"
    <*> o .: "ts"

instance ToJSON MessageEdited where
  toJSON (MessageEdited user ts) = object ["user" .= user, "ts" .= ts]

instance FromJSON MessageReaction where
  parseJSON = withObject "message reaction object" $ \ o -> MessageReaction
    <$> o .: "name"
    <*> o .: "count"
    <*> o .: "users"

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
              "channel_marked"          -> RtmChannelMarked <$> recur
              "channel_created"         -> RtmChannelCreated <$> o .: "channel"
              "channel_deleted"         -> RtmChannelDeleted <$> o .: "channel"
              "channel_rename"          -> RtmChannelRenamed <$> o .: "channel"
              "channel_archive"         -> RtmChannelArchive <$> recur
              "channel_unarchive"       -> RtmChannelUnarchive <$> recur
              "channel_history_changed" -> RtmChannelHistoryChanged <$> recur
              "presence_change"         -> RtmPresenceChange <$> recur
              "manual_presence_change"  -> RtmManualPresenceChange <$> o .: "presence"
              "user_typing"             -> RtmUserTyping <$> recur
              "pref_change"             -> RtmPrefChange <$> recur
              "user_change"             -> RtmUserChange <$> o .: "user"
              "star_added"              -> RtmStarAdded <$> recur
              "star_removed"            -> RtmStarRemoved <$> recur
              "emoji_changed"           -> RtmEmojiChanged <$> o .: "event_ts"
              "commands_changed"        -> RtmCommandsChanged <$> o .: "event_ts"
              "team_pref_change"        -> RtmTeamPrefChange <$> recur
              "bot_added"               -> RtmBotAdded <$> o .: "bot"
              "bot_changed"             -> RtmBotChanged <$> o .: "bot"
              "accounts_changed"        -> pure RtmAccountsChanged
              "incoming_message"        -> RtmSendMessage <$> recur
              other                     -> fail . unpack $ "unknown RTM event type " <> other

instance ToJSON RtmEvent where
  toJSON event = case event of
                  RtmSendMessage msg -> toJSON msg
                  RtmMessage message -> toJSON message
                  RtmHello           -> object ["type" .= ("hello" :: Text)]

instance FromJSON (ChatMarked a) where
  parseJSON = withObject "channel / im / group marked event" $ \ o -> ChatMarked
    <$> o .: "channel"
    <*> o .: "ts"

instance FromJSON (ChatUser a) where
  parseJSON = withObject "channel and user from event" $ \ o -> ChatUser
    <$> o .: "channel"
    <*> o .: "user"

instance FromJSON (ChatRenamed a) where
  parseJSON = withObject "channel and new name from event" $ \ o -> ChatRenamed
    <$> o .: "id"
    <*> o .: "name"

instance FromJSON (ChatHistoryChanged a) where
  parseJSON = withObject "channel history changed event" $ \ o -> ChatHistoryChanged
    <$> o .: "latest"
    <*> o .: "ts"
    <*> o .: "event_ts"


instance FromJSON PresenceChange where
  parseJSON = withObject "presence change event" $ \ o -> PresenceChange
    <$> o .: "user"
    <*> o .: "presence"

instance FromJSON UserTyping where
  parseJSON = withObject "user typing event" $ \ o -> UserTyping
    <$> o .: "user"
    <*> o .: "channel"

instance FromJSON PrefChange where
  parseJSON = withObject "pref change event" $ \ o -> PrefChange
    <$> o .: "name"
    <*> o .: "value"

instance FromJSON Star where
  parseJSON = withObject "star event" $ \ o -> Star
    <$> o .: "user"
    <*> o .: "item"
    <*> o .: "event_ts"

instance FromJSON StarItem where
  parseJSON = withObject "starred item reference" $ \ o -> o .: "type" >>= pure . asText >>= \ case
    "message"      -> StarItemMessage     <$> o .: "message"
    "channel"      -> StarItemChannel     <$> o .: "channel"
    other          -> fail . unpack $ "unknown starrable item type " <> other

instance FromJSON TeamDomainChange where
  parseJSON = withObject "team domain change event" $ \ o -> TeamDomainChange
    <$> o .: "url"
    <*> o .: "domain"

instance FromJSON EmailDomainChanged where
  parseJSON = withObject "email domain changed event" $ \ o -> EmailDomainChanged
    <$> o .: "email_domain"
    <*> o .: "event_ts"

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
