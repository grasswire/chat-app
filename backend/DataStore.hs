{-# LANGUAGE OverloadedStrings #-}

module DataStore 
  (
    RedisAction
  , runRedisAction
  , numUsersPresent
  , setChannelPresence
  , incrChannelPresence
  , decrChannelPresence
  , getPresenceForChannels    
  ) where 

import           ClassyPrelude
import           Database.Redis
import qualified Types as TP
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as C8
import           Taplike.Schema (ChannelSlug, unSlug)
import Data.Typeable
import Control.Monad.Catch 
import Control.Monad.IO.Class

type RedisAction a = ExceptT Reply (ReaderT Connection IO) a

runRedisAction :: Connection -> RedisAction a -> IO (Either Reply a)
runRedisAction conn action = runReaderT (runExceptT action) conn

withRedisExcept :: (Connection -> IO (Either Reply a)) -> RedisAction a
withRedisExcept = ExceptT . ReaderT

channelPresenceSetKey :: ByteString
channelPresenceSetKey = C8.pack "channels:presence"

channelPresenceKey :: ChannelSlug -> ByteString
channelPresenceKey =  encodeUtf8 . (\slug -> "channel:" <> slug <> ":presence") . unSlug

numUsersPresent :: ChannelSlug -> RedisAction (Maybe TP.NumberUsersPresent)
numUsersPresent key = listToMaybe <$> getPresenceForChannels [key]

toUsersPresent :: Double -> TP.NumberUsersPresent
toUsersPresent = TP.NumberUsersPresent . fromIntegral . round

-- setChannelPresence :: Integer -> ChannelSlug -> RedisAction Integer
-- setChannelPresence score channelId = withRedisExcept $ \conn -> do
--   let key = channelPresenceKey channelId
--   runRedis conn $ do
--     currentPresence <- hget channelPresenceSetKey key
--     case currentPresence of 
--       Left err -> pure $ Left err
--       Right x -> 
--         case x of 
--           Just p -> do
--             let maybeInteger = fst <$> C8.readInteger p
--             case maybeInteger of 
--               Just i -> hincrby channelPresenceSetKey key (score - i)
--               _ -> pure $ Left (Error $ C8.pack "could not parse hash field as an integer")
--           _ -> pure $ Left (Error $ C8.pack "received Nothing for hash field")    

data RedisException = RedisException String 
    deriving (Show, Typeable)

instance Exception RedisException

setChannelPresence :: (MonadIO m, MonadThrow m) => Integer -> ChannelSlug -> ReaderT Connection m Integer
setChannelPresence score channelId = do
  let key = channelPresenceKey channelId
  conn <- ask
  currentPresence <- liftIO $ runRedis conn (hget channelPresenceSetKey key)
  case currentPresence of 
    Left e -> throwM (RedisException $ show e)
    Right x -> case x of
      Nothing -> throwM (RedisException "received Nothing for hash field")
      Just p -> do 
        let maybeInteger = fst <$> C8.readInteger p
        case maybeInteger of 
          Just i -> do 
            result <- liftIO $ runRedis conn (hincrby channelPresenceSetKey key (score - i))
            either (throwM . RedisException . show) return result
          Nothing -> throwM (RedisException "could not parse hash field as an integer")  
              
incrChannelPresence :: ChannelSlug -> RedisAction Integer
incrChannelPresence channelId = withRedisExcept $ \conn -> do
  let action = hincrby channelPresenceSetKey (channelPresenceKey channelId) 1
  runRedis conn action

decrChannelPresence :: ChannelSlug -> RedisAction Integer
decrChannelPresence channelId = withRedisExcept $ \conn -> do
  let action = hincrby channelPresenceSetKey (channelPresenceKey channelId) (-1)
  runRedis conn action

getPresenceForChannels :: [ChannelSlug] -> RedisAction [TP.NumberUsersPresent]
getPresenceForChannels channelIds = withRedisExcept $ \conn -> do 
    let fields = fmap channelPresenceKey channelIds
        action = hmget channelPresenceSetKey fields 
        defaultPresence = TP.NumberUsersPresent 0
    result <- runRedis conn action 
    case result of 
      Right xs -> return $ Right $ fmap (maybe defaultPresence (\x -> fromMaybe defaultPresence (TP.NumberUsersPresent <$> readMay (C8.unpack x)))) xs
      _        -> return $ Right (replicate (length channelIds) defaultPresence)