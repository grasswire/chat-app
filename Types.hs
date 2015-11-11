module Types where

import Data.Text.Lazy
import Network.WebSockets hiding (Message)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.String (IsString, fromString)
import Data.Semigroup

type ClientName = Text
type ChannelName = Text

-- {"type":"message","channel":"D029PF7RP","text":"hi","id":17}
data Message
    = Notice Text
    | Tell ClientName Text
    | Broadcast ChannelName ClientName Text
    | Command Text

instance WebSocketsData Message where
  fromLazyByteString bs = Broadcast "general" "levi-client"  (decodeUtf8 bs)
  toLazyByteString msg = case msg of
                          Broadcast chanName clientName t -> encodeUtf8 t

instance IsString Message where
  fromString msg = Broadcast "general" "levi-client" (pack msg)

instance Semigroup Message where
  (<>) (Broadcast chan client msg1) (Broadcast _ _ msg2) = Broadcast chan client (msg1 <> msg2)
