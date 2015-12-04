module DataStore where

import           ClassyPrelude
import           Database.Redis
import qualified Types as TP
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Model
import           Database.Persist (Key)
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as C8
import           Data.Binary (encode)
import qualified Data.Binary as Bin

type RedisAction a = ExceptT Reply (ReaderT Connection IO) a

runRedisAction :: Connection -> RedisAction a -> IO (Either Reply a)
runRedisAction conn action = runReaderT (runExceptT action) conn

channelPresenceSetKey :: ByteString
channelPresenceSetKey = C8.pack "chan_set"

mkChannelPresenceSetValue :: Key Channel -> ByteString
mkChannelPresenceSetValue = toStrict . encode . fromSqlKey

numUsersPresent :: Key Channel -> RedisAction (Maybe TP.NumberUsersPresent)
numUsersPresent key = ExceptT $ ReaderT $ \conn -> do
    score <- runRedis conn $ zscore channelPresenceSetKey (mkChannelPresenceSetValue key)
    return $ fmap toUsersPresent <$> score

channelsByPresenceDesc :: Integer -> RedisAction [(Key Channel, TP.NumberUsersPresent)]
channelsByPresenceDesc maxChans = do
   res <- ExceptT $ ReaderT $ \conn -> runRedis conn $ zrangeWithscores channelPresenceSetKey 0 (maxChans - 1)
   return ((\(chanKey, score) -> (keyToChanKey chanKey, toUsersPresent score)) <$> res)

keyToChanKey :: ByteString -> Key Channel
keyToChanKey = toSqlKey . Bin.decode . fromStrict

toUsersPresent :: Double -> TP.NumberUsersPresent
toUsersPresent = TP.NumberUsersPresent . fromIntegral . round
