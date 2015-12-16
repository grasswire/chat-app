{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Int (Int64, Int32)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable
import Data.Word (Word64)
import Data.UUID

newtype ChannelColor =
  ChannelColor { unChannelColor :: Text }
  deriving (Eq, Show, Typeable, Read)

data NewChannel = NewChannel
  { newChannelTitle :: ChannelTitle
  , newChannelTopic :: ChannelTopic
  , newChannelColor :: ChannelColor
  } deriving (Eq, Show, Typeable)

newtype UserId =
  UserId { unUserId :: Int64 }
  deriving (Eq, Show, Typeable)
  
newtype MessageText = MessageText { unMessageText :: Text}

data ChannelCreatedRp = ChannelCreatedRp
  { channelCreatedRpChannel     :: Channel
  , channelCreatedRpId          :: Int64
  , channelCreatedRpChannelSlug :: ChannelSlug
  }

newtype TS = TS { unTS :: Text } deriving (Eq, Ord)

newtype ID a = ID { unID :: Text } deriving (Eq, Ord)

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
  { heartBeatUser    :: UserId
  , heartBeatChannel :: ChannelSlug
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


newtype MessageId = 
  MessageId { unMessageId :: UUID } 
  deriving (Eq, Show, Typeable) 

newtype ChannelSlug =
  ChannelSlug { unChannelSlug :: Text }
  deriving (Eq, Show, Typeable, Read)

newtype ChannelTitle =
  ChannelTitle { unChannelTitle :: Text }
  deriving (Eq, Show, Typeable, Ord)

newtype ChannelTopic =
  ChannelTopic { unChannelTopic :: Text }
  deriving (Eq, Show, Typeable, Ord)

newtype TwitterUserId =
  TwitterUserId { unTwitterUserId :: Int64 }
  deriving (Eq, Show, Typeable)

newtype TwitterScreenName =
  TwitterScreenName { unTwitterScreenName :: Text }
  deriving (Eq, Show, Typeable)

newtype ProfileImageUrl =
  ProfileImageUrl { unProfileImageUrl :: Text }
  deriving (Eq, Show, Typeable)

data User = User
  { userUserId            :: UserId
  , userTwitterUserId     :: TwitterUserId
  , userTwitterScreenName :: TwitterScreenName
  , userProfileImageUrl   :: ProfileImageUrl
  } deriving (Eq, Show)

newtype NumberUsersPresent =
  NumberUsersPresent { unNumberUsersPresent :: Int64 }
  deriving (Eq, Show, Typeable, Ord)

data Channel = Channel
  { channelCreator         :: UserId
  , channelCreated         :: UTCTime
  , channelTopic           :: ChannelTopic
  , channelChannelSlug     :: ChannelSlug
  , channelTitle           :: ChannelTitle
  , channelNumUsersPresent :: NumberUsersPresent
  , channelColor           :: ChannelColor  
  }

data NewMessageLike = NewMessageLike 
  { messageLikeMessageId :: MessageId
  , messageLikeChannel   :: ChannelSlug
  } deriving (Eq, Show)

data OkResponse = OkResponse deriving (Eq, Show)
