module Taplike.Shared where

import ClassyPrelude
import           TextShow.TH (deriveTextShow)
import           Data.Aeson ((.:), (.:?), (.=), (.!=), Value(Object, String), Object, FromJSON(parseJSON), ToJSON(toJSON), object, withText, withObject, withScientific, withText)

-- import Data.Text

newtype ID a = ID { unID :: Text } deriving (Eq, Ord)

instance FromJSON (ID a) where
  parseJSON = withText "id" $ pure . ID

instance ToJSON (ID a) where
  toJSON = String . unID
deriveTextShow ''ID

data Chat
