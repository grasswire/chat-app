module Types where

import ClassyPrelude

import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as TL
import Network.WebSockets hiding (Message)
import Data.String (IsString, fromString)
import Data.Semigroup
import Data.Aeson (encode, decode, Value(..))

type ClientName = TL.Text
type ChannelName = TL.Text

-- {"type":"message","channel":"D029PF7RP","TL.Text":"hi","id":17}
data Message
    = Notice TL.Text
    | Tell ClientName TL.Text
    | Broadcast ChannelName ClientName TL.Text
    | Command TL.Text

-- fromLazyByteString :: ByteString -> a
instance WebSocketsData Value where
  fromLazyByteString = fromJust . decode
  toLazyByteString = encode

-- instance IsString Message where
--   fromString msg = Broadcast "general" "levi-client" (pack msg)
--
-- instance Semigroup Message where
--   (<>) (Broadcast chan client msg1) (Broadcast _ _ msg2) = Broadcast chan client (msg1 <> msg2)
