module DataStore where

import           ClassyPrelude
import           Database.Redis
import qualified Types as TP
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as C8
import           Data.Binary (encode)
import Taplike.Schema (ChannelSlug)

type RedisAction a = ExceptT Reply (ReaderT Connection IO) a

runRedisAction :: Connection -> RedisAction a -> IO (Either Reply a)
runRedisAction conn action = runReaderT (runExceptT action) conn

withRedisExcept :: (Connection -> IO (Either Reply a)) -> RedisAction a
withRedisExcept = ExceptT . ReaderT

channelPresenceSetKey :: ByteString
channelPresenceSetKey = C8.pack "chan_presence_hash"

mkChannelPresenceSetValue :: ChannelSlug -> ByteString
mkChannelPresenceSetValue = encodeUtf8 . pack . show 

chanKey2bs :: ChannelSlug -> ByteString
chanKey2bs = mkChannelPresenceSetValue

numUsersPresent :: ChannelSlug -> RedisAction (Maybe TP.NumberUsersPresent)
numUsersPresent key = withRedisExcept $ \conn -> do
    score <- runRedis conn $ zscore channelPresenceSetKey (mkChannelPresenceSetValue key)
    return $ fmap toUsersPresent <$> score

toUsersPresent :: Double -> TP.NumberUsersPresent
toUsersPresent = TP.NumberUsersPresent . fromIntegral . round

setChannelPresence :: Integer -> ChannelSlug -> RedisAction Bool
setChannelPresence score channelId = withRedisExcept $ \conn -> do
  let action = hset channelPresenceSetKey (chanKey2bs channelId) (toStrict $ encode score)
  runRedis conn action

incrChannelPresence :: ChannelSlug -> RedisAction Integer
incrChannelPresence channelId = withRedisExcept $ \conn -> do
  let action = hincrby channelPresenceSetKey (chanKey2bs channelId) 1
  runRedis conn action

decrChannelPresence :: ChannelSlug -> RedisAction Integer
decrChannelPresence channelId = withRedisExcept $ \conn -> do
  let action = hincrby channelPresenceSetKey (chanKey2bs channelId) (-1)
  runRedis conn action

getPresenceForChannels :: [ChannelSlug] -> RedisAction [TP.NumberUsersPresent]
getPresenceForChannels channelIds = withRedisExcept $ \conn -> do 
    let fields = fmap chanKey2bs channelIds
        action = hmget channelPresenceSetKey fields 
        defaultPresence = TP.NumberUsersPresent 0
    result <- runRedis conn action 
    case result of 
      Right xs -> return $ Right $ fmap (maybe defaultPresence (\x -> fromMaybe defaultPresence (TP.NumberUsersPresent <$> readMay (C8.unpack x)))) xs
      _        -> return $ Right (replicate (length channelIds) defaultPresence) 
  

