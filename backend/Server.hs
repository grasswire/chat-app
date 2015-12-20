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
 ) where

import           ClassyPrelude              hiding ((<>))
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           Data.Set (Set)
import qualified Data.Set as Set
import           Types                      (RtmEvent(..))
import           Model.Instances            ()
import           Model                      (Message(..))
import           Database.Redis             hiding (Message, subscribe)
import qualified Database.Redis             as Redis
import qualified Data.ByteString.Lazy.Char8 as LC8
import           DataStore
import           Control.Monad.Trans.Except
import qualified Data.Aeson                 as Aeson
import           Control.Concurrent         (forkIO)
import           Taplike.Schema             (ChannelSlug, unSlug, ChannelSlug(..))
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import           Control.Concurrent.Chan.Unagi.Bounded (InChan, OutChan)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.UUID.V4 (nextRandom)
import           Data.UUID (UUID)
import qualified Data.Map.Strict as Map
import qualified Data.Map        as M

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
  
newClient :: IO Client
newClient = do
  randId            <- nextRandom
  (inChan, outChan) <- Unagi.newChan 1000 
  return (Client randId inChan outChan)

newServer :: Connection -> IO Server
newServer conn = atomically $ Server conn <$> newTVar M.empty 

subscribe :: NonEmpty ChannelSlug -> Client -> Server -> IO () 
subscribe chans client@Client{..} server@Server{..} = do 
  newSubs <- (\ c -> filter (`Map.notMember` c) (toList chans)) <$> atomically (readTVar serverClients)
  atomically $ modifyTVar serverClients (\mMap -> foldl' (\m c -> 
      case Map.lookup c m of  
        Nothing -> Map.insert c (Set.singleton client) m 
        Just clients -> Map.update (Just . Set.insert client) c m) mMap chans)
  void $ forkIO $ runRedis serverConnection (pubSub (Redis.subscribe (chanId2Bs <$> newSubs)) (messageCallback server))
  
readMessage :: Client -> IO RtmEvent  
readMessage client@Client{..} = Unagi.readChan clientOutChan

messageCallback :: Server -> Redis.Message -> IO PubSub
messageCallback server@Server{..} msg = do
  let slug = ChannelSlug $ decodeUtf8 (msgChannel msg)
  inChans <- fmap clientInChan <$> lookupSubscribers slug server
  unless (null inChans)
        (case Aeson.eitherDecode (LC8.fromStrict $ msgMessage msg) :: Either String RtmEvent of
          Right broadcastMsg -> forM_ inChans (`Unagi.writeChan` broadcastMsg)
          Left _  -> return ())
  return mempty
  
lookupSubscribers :: ChannelSlug -> Server -> IO [Client]
lookupSubscribers chanSlug server@Server{..} = do 
  clientMap <- atomically $ readTVar serverClients
  (return . maybe [] Set.toList) (Map.lookup chanSlug clientMap)
  
broadcastEvent :: ChannelSlug -> RtmEvent -> RedisAction Integer
broadcastEvent chanId event = ExceptT $ ReaderT $ \conn -> runRedis conn $ publish (chanId2Bs chanId) (BL.toStrict $ Aeson.encode event)

chanId2Bs :: ChannelSlug -> BS.ByteString
chanId2Bs = encodeUtf8 . unSlug