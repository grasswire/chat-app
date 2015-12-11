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
import Control.Arrow ((***))
import Prelude (read)

type RedisAction a = ExceptT Reply (ReaderT Connection IO) a

runRedisAction :: Connection -> RedisAction a -> IO (Either Reply a)
runRedisAction conn action = runReaderT (runExceptT action) conn

withRedisExcept :: (Connection -> IO (Either Reply a)) -> RedisAction a
withRedisExcept = ExceptT . ReaderT

channelPresenceSetKey :: ByteString
channelPresenceSetKey = C8.pack "chan_presence_hash"

mkChannelPresenceSetValue :: Key Channel -> ByteString
mkChannelPresenceSetValue = encodeUtf8 . pack . show . fromSqlKey

chanKey2bs :: Key Channel -> ByteString
chanKey2bs = mkChannelPresenceSetValue

numUsersPresent :: Key Channel -> RedisAction (Maybe TP.NumberUsersPresent)
numUsersPresent key = withRedisExcept $ \conn -> do
    score <- runRedis conn $ zscore channelPresenceSetKey (mkChannelPresenceSetValue key)
    return $ fmap toUsersPresent <$> score

channelsByPresenceDesc :: Integer -> RedisAction [(Key Channel, TP.NumberUsersPresent)]
channelsByPresenceDesc maxChans = do
   res <- ExceptT $ ReaderT $ \conn -> runRedis conn $ zrangeWithscores channelPresenceSetKey 0 (maxChans - 1)
   return ((keyToChanKey *** toUsersPresent) <$> res)

keyToChanKey :: ByteString -> Key Channel
keyToChanKey = toSqlKey . Bin.decode . fromStrict

toUsersPresent :: Double -> TP.NumberUsersPresent
toUsersPresent = TP.NumberUsersPresent . fromIntegral . round

setChannelPresence :: Integer -> Key Channel -> RedisAction Bool
setChannelPresence score channelId = withRedisExcept $ \conn -> do
  let action = hset channelPresenceSetKey (chanKey2bs channelId) (toStrict $ encode score)
  runRedis conn $ action

incrChannelPresence :: Key Channel -> RedisAction Integer
incrChannelPresence channelId = withRedisExcept $ \conn -> do
  let action = hincrby channelPresenceSetKey (chanKey2bs channelId) 1
  runRedis conn $ action

decrChannelPresence :: Key Channel -> RedisAction Integer
decrChannelPresence channelId = withRedisExcept $ \conn -> do
  let action = hincrby channelPresenceSetKey (chanKey2bs channelId) (-1)
  runRedis conn $ action

getPresenceForChannels :: [Key Channel] -> RedisAction [TP.NumberUsersPresent]
getPresenceForChannels channelIds = withRedisExcept $ \conn -> do 
    let fields = fmap chanKey2bs channelIds
    let action = hmget channelPresenceSetKey fields 
    result <- runRedis conn $ action 
    case result of 
      Right xs -> return $ Right $ fmap (maybe (TP.NumberUsersPresent 0) (TP.NumberUsersPresent . read . C8.unpack)) xs
      _        -> return $ Right (replicate (length channelIds) (TP.NumberUsersPresent 0)) 
  

