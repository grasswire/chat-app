{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Server
  ( Server
  , Client 
  , LeaveReason(..)
  , JoinReason(..)
  , broadcastEvent
  , newClient 
  , subscribe 
  , readMessage 
  , newServer
  , notifyChannelJoin
  ) where

import           ClassyPrelude hiding ((<>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import           Types hiding (ChannelSlug)
import qualified Types 
import           Model.Instances ()
import           Database.Redis hiding (Message, subscribe)
import qualified Database.Redis as Redis
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Aeson as Aeson
import           Control.Concurrent (forkIO)
import           Taplike.Schema (ChannelSlug, unSlug, ChannelSlug(..))
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import           Control.Concurrent.Chan.Unagi.Bounded (InChan, OutChan)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.UUID.V4 (nextRandom)
import           Data.UUID (UUID)
import qualified Data.Map.Strict as Map
import qualified Data.Map as M

type ClientId = UUID

data LeaveReason
    = LeaveReasonLeft
    | LeaveReasonDisconnected

data JoinReason
    = JoinReasonJoined
    | JoinReasonConnected

data Server = Server 
  { serverConnection :: Connection 
  , serverClients    :: TVar (Map ChannelSlug (Set Client)) 
  }

data Client = Client
    { clientId       :: ClientId
    , clientInChan   :: InChan RtmEvent
    , clientOutChan  :: OutChan RtmEvent
    }
    
instance Eq Client where 
  c1 == c2 = clientId c1 == clientId c2
  
instance Ord Client where 
  compare c1 c2 = clientId c1 `compare` clientId c2
  
newClient :: MonadIO m => m Client
newClient = do
  randId            <- liftIO nextRandom
  (inChan, outChan) <- liftIO (Unagi.newChan 1000)
  return (Client randId inChan outChan)

newServer :: MonadIO m => Connection -> m Server
newServer conn = liftIO (atomically $ Server conn <$> newTVar M.empty)

subscribe :: MonadIO m => NonEmpty ChannelSlug -> Client -> Server -> m () 
subscribe chans client@Client{..} server@Server{..} = do 
  newSubs <- (\c -> filter (`Map.notMember` c) (toList chans)) <$> liftIO (atomically (readTVar serverClients))
  liftIO (atomically $ modifyTVar serverClients (\mMap -> foldl' (\m c -> 
      case Map.lookup c m of  
        Nothing      -> Map.insert c (Set.singleton client) m 
        Just clients -> Map.update (Just . Set.insert client) c m) mMap chans))
  liftIO (void $ forkIO $ runRedis serverConnection (pubSub (Redis.subscribe (chanId2Bs <$> newSubs)) (messageCallback server)))
  
readMessage :: MonadIO m => Client -> m RtmEvent  
readMessage client@Client{..} = liftIO $ Unagi.readChan clientOutChan

messageCallback :: MonadIO m => Server -> Redis.Message -> m PubSub
messageCallback server@Server{..} msg = do
  let slug = ChannelSlug $ decodeUtf8 (msgChannel msg)
  inChans <- fmap clientInChan <$> lookupSubscribers slug server
  unless (null inChans)
        (case Aeson.eitherDecode (LC8.fromStrict $ msgMessage msg) :: Either String RtmEvent of
          Right broadcastMsg -> liftIO $ forM_ inChans (`Unagi.writeChan` broadcastMsg)
          Left _             -> return ())
  return mempty
  
lookupSubscribers :: MonadIO m => ChannelSlug -> Server -> m [Client]
lookupSubscribers chanSlug server@Server{..} = do 
  clientMap <- atomically $ readTVar serverClients
  (return . maybe [] Set.toList) (Map.lookup chanSlug clientMap)
  
broadcastEvent :: MonadIO m => ChannelSlug -> RtmEvent -> Server -> m (Either Reply Integer)
broadcastEvent chanId event server@Server{..} = liftIO $ runRedis serverConnection $ publish (chanId2Bs chanId) 
                                                                                     (BL.toStrict $ Aeson.encode event)

notifyChannelJoin :: MonadIO m => ChannelSlug -> User -> Server -> m (Either Reply Integer)
notifyChannelJoin slug user server@Server{..} = do 
  now <- liftIO getCurrentTime
  liftIO $ runRedis serverConnection $ publish (chanId2Bs slug) 
                                       (BL.toStrict $ Aeson.encode 
                                       (RtmChannelJoin (ChannelJoin (Types.ChannelSlug $ unSlug slug) user now)))

chanId2Bs :: ChannelSlug -> BS.ByteString
chanId2Bs = encodeUtf8 . unSlug