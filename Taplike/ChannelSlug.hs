module Taplike.ChannelSlug where

import Yesod.Core.Dispatch (PathPiece)
import ClassyPrelude
import Data.Aeson(Value(..), ToJSON(toJSON), FromJSON(parseJSON))
import Data.Aeson.Types (typeMismatch)
import Database.Persist.Sql
import TextShow.TH (deriveTextShow)

newtype ChannelSlug = ChannelSlug {unSlug :: Text}
    deriving (PathPiece, Show, Eq, Read)

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

deriveTextShow ''ChannelSlug
