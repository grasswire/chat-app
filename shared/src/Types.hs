{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Int (Int64, Int32)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable
import Data.Word (Word64)

newtype NewChannelTitle =
  NewChannelTitle { unNewChannelTitle :: Text }
  deriving (Eq, Show, Typeable, Read)

newtype NewChannelTopic =
  NewChannelTopic { unNewChannelTopic :: Text }
  deriving (Eq, Show, Typeable)

data NewChannel = NewChannel
  { newChannelTitle :: NewChannelTitle
  , newChannelTopic :: NewChannelTopic
  } deriving (Eq, Show, Typeable)

newtype UserId =
  UserId { unUserId :: Int64 }
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
  TwitterScreenName { unTwitterScreenName :: Int64 }
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
  , channelSlug            :: ChannelSlug
  , channelTitle           :: ChannelTitle
  , channelNumUsersPresent :: NumberUsersPresent
  }
