{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Taplike.Schema where

import Yesod.Core.Dispatch (PathPiece)
import ClassyPrelude
import Data.Aeson(Value(..), ToJSON(toJSON), FromJSON(parseJSON))
import Data.Aeson.Types (typeMismatch)
import Database.Persist.Sql
import TextShow.TH (deriveTextShow)
import TextShow (showt)
import Control.Error
import Control.Lens
import Data.ByteString (ByteString)
import Data.UUID
import System.Random

newtype ChannelSlug = ChannelSlug {unSlug :: Text}
    deriving (PathPiece, Show, Eq, Read, Ord)

instance ToJSON ChannelSlug where
  toJSON (ChannelSlug slug) = String slug

instance FromJSON ChannelSlug where
  parseJSON (String s) = pure $ ChannelSlug s
  parseJSON invalid = typeMismatch "ChannelSlug" invalid

instance PersistField ChannelSlug where
    toPersistValue (ChannelSlug slug) = toPersistValue slug
    fromPersistValue (PersistText txt) = Right $ ChannelSlug txt
    fromPersistValue x = Left $ "Not a PersistText " ++ pack (show x)

instance PersistFieldSql ChannelSlug where
    sqlType _ =  SqlString

newtype MessageUUID = MessageUUID {
      _messageUuid :: UUID
    } deriving (Show, Eq, Ord, Random)

makeLenses ''MessageUUID

instance PersistFieldSql MessageUUID where
  sqlType = const $ SqlOther "uuid"

instance PersistField MessageUUID where
  toPersistValue = toPersistValueUUID messageUuid
  fromPersistValue = fromPersistValueUUID messageUuid

_ASCIIBytes :: Prism' ByteString UUID
_ASCIIBytes = prism toASCIIBytes $ \bs -> note bs $ fromASCIIBytes bs

toPersistValueUUID :: Iso' a UUID -> a -> PersistValue
toPersistValueUUID i a = PersistDbSpecific $ a ^. i . re _ASCIIBytes

fromPersistValueUUID :: Iso' a UUID -> PersistValue -> Either Text a
fromPersistValueUUID i (PersistDbSpecific bs) =
  note "Could not parse UUID" $ bs ^? _ASCIIBytes . from i
fromPersistValueUUID _ x = Left $ "Invalid value for UUID: " <> showT x

showT = pack . show

deriveTextShow ''ChannelSlug
