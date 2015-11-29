{-# OPTIONS_GHC -fno-warn-orphans #-}
module Taplike.TextShowOrphans where

import           ClassyPrelude
import           Control.Wire.Unsafe.Event (Event, event)
import           Data.Aeson (Value)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as MS
import           TextShow (FromStringShow(FromStringShow), TextShow(showbPrec))
import           TextShow.TH (deriveTextShow)
import           Data.UUID

-- powerful orphan instance time!
instance TextShow Value where
  showbPrec prec = showbPrec prec . FromStringShow
instance (TextShow k, TextShow v) => TextShow (HM.HashMap k v) where
  showbPrec prec = ("HM.fromList " <>) . showbPrec prec . HM.toList
instance (TextShow k, TextShow v) => TextShow (MS.Map k v) where
  showbPrec prec = ("MS.fromList " <>) . showbPrec prec . MS.toList
instance TextShow a => TextShow (Seq a) where
  showbPrec prec = ("Seq.fromList " <>) . showbPrec prec . toList
instance TextShow a => TextShow (Event a) where
  showbPrec prec = event "NoEvent" (("Event " <>) . showbPrec prec)

deriveTextShow ''UUID
