module DataStore where

import ClassyPrelude
import qualified Database.Redis as Redis
import           Database.Redis (runRedis, zscore)
import qualified Types as TP
import           Database.Persist.Sql (fromSqlKey)
import Model
import Database.Persist (Key)
import Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as C8
import           Data.Binary (encode)

type RedisAction a = ExceptT Redis.Reply (ReaderT Redis.Connection IO) a

runRedisAction :: Redis.Connection -> RedisAction a -> IO (Either Redis.Reply a)
runRedisAction conn action = runReaderT (runExceptT action) conn

channelPresenceSetKey :: ByteString
channelPresenceSetKey = C8.pack "chan_set"

mkChannelPresenceSetValue :: Key Channel -> ByteString
mkChannelPresenceSetValue = toStrict . encode . fromSqlKey

numUsersPresent :: Key Channel -> RedisAction (Maybe TP.NumberUsersPresent)
numUsersPresent key = ExceptT $ ReaderT $ \conn -> do
    score <- runRedis conn $ zscore channelPresenceSetKey (mkChannelPresenceSetValue key)
    return $ fmap toUsersPresent <$> score
    where toUsersPresent = TP.NumberUsersPresent . fromIntegral . round
