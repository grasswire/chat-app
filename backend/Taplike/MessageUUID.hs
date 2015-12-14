{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module 

import Control.Error
import Control.Lens
import Data.ByteString (ByteString)
import Data.UUID
import System.Random

newtype PersonUUID = PersonUUID {
      _personUuid :: UUID
    } deriving (Show, Eq, Ord, Random)

makeLenses ''PersonUUID

instance PersistFieldSql PersonUUID where
  sqlType = const $ SqlOther "uuid"

instance PersistField PersonUUID where
  toPersistValue = toPersistValueUUID personUuid
  fromPersistValue = fromPersistValueUUID personUuid

_ASCIIBytes :: Prism' ByteString UUID
_ASCIIBytes = prism toASCIIBytes $ \bs -> note bs $ fromASCIIBytes bs

toPersistValueUUID :: Iso' a UUID -> a -> PersistValue
toPersistValueUUID i a = PersistDbSpecific $ a ^. i . re _ASCIIBytes

fromPersistValueUUID :: Iso' a UUID -> PersistValue -> Either Text a
fromPersistValueUUID i (PersistDbSpecific bs) =
  note "Could not parse UUID" $ bs ^? _ASCIIBytes . from i
fromPersistValueUUID _ x = Left $ "Invalid value for UUID: " <> showT x