{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Server where

import           ClassyPrelude              hiding ((<>))
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           Types                      (RtmEvent(..))
import           Model.Instances            ()
import           Model                      (Message(..))
import           Database.Redis             hiding (Message)
import qualified Database.Redis             as Redis
import qualified Data.ByteString.Lazy.Char8 as LC8
import           DataStore
import           Control.Monad.Trans.Except
import qualified Data.Aeson                 as Aeson
import           Control.Concurrent         (forkIO)
import           Taplike.Schema             (ChannelSlug, unSlug, ChannelSlug(..))
import Debug.Trace
import qualified Data.Text.IO as TIO

type ClientId = Int64

data LeaveReason
    = LeaveReasonLeft
    | LeaveReasonDisconnected

data JoinReason
    = JoinReasonJoined
    | JoinReasonConnected

-- Note there is no notion of DMs here. That would have to be a specific, fixed channel
data Client = Client
    { clientName          :: ClientId
    , clientKicked        :: TVar (Maybe (String, String)) -- should be Map String String since multiple channels
    , clientChans         :: TVar (Map Channel (TChan Message)) -- client can be in multiple channels
    }

newClient :: ClientId -> STM Client
newClient name  = do
    broadcastChan <- newTVar M.empty
    kicked        <- newTVar Nothing
    return Client
        { clientName    = name
        , clientKicked  = kicked
        , clientChans   = broadcastChan
        }

-- Chat channel datatype, not to be confused with a TChan.
data Channel = Channel
    { channelId            :: ChannelSlug
    , channelClients       :: TVar (S.Set ClientId)
    , channelBroadcastChan :: TChan RtmEvent
    }

newChannel :: ChannelSlug -> STM Channel
newChannel name = Channel name <$> newTVar S.empty <*> newBroadcastTChan

-- Send a Notice to the channel.
chanNotify :: Channel -> RtmEvent -> STM ()
chanNotify = chanMessage

-- Send a Message to the channel.
chanMessage :: Channel -> RtmEvent -> STM ()
chanMessage = writeTChan . channelBroadcastChan

-- Notify the channel a client has connected.
chanNotifyHasConnected :: Channel -> ClientId -> STM ()
chanNotifyHasConnected chan name = chanNotify chan RtmHello

data Server = Server
    { serverChannels       :: TVar (Map ChannelSlug Channel)
    , serverClients        :: TVar (Map ClientId Client)
    , serverSubscriptions  :: TVar PubSub
    }

newServer :: IO Server
newServer = atomically $ Server <$> newTVar M.empty <*> newTVar M.empty <*> newTVar mempty

lookupOrCreateChannel :: Connection -> Server -> ChannelSlug -> IO Channel
lookupOrCreateChannel conn server@Server{..} name = do
      channel <- atomically $ lookupChannel server name
      let pubSubChan = chanId2Bs name
      case channel of
        Nothing -> do
          newServerChan <- atomically $ do
              chan <- newChannel name
              modifyTVar serverChannels . M.insert name $ chan
              return chan
          void $ forkIO $ runRedis conn (pubSub (subscribe [pubSubChan]) (messageCallback server))
          return newServerChan
        Just chan -> return chan

lookupOrCreateChannels :: Connection -> Server -> [ChannelSlug] -> IO [Channel]
lookupOrCreateChannels conn server@Server{..} channels = do
      chanLookup <- atomically $ traverse (lookupChannel server) channels
      let xs = chanLookup `zip` channels 
          (found, notfound) = partition (isJust . fst) xs
      let pubSubChans = (chanId2Bs . snd) <$> notfound 
      newServerChannels <- forM (snd <$> notfound) (\name -> atomically $ do
              chan <- newChannel name
              modifyTVar serverChannels . M.insert name $ chan
              return chan)
      void $ forkIO $ runRedis conn (pubSub (subscribe pubSubChans) (messageCallback server))  
      return (newServerChannels ++ (catMaybes (fst <$> found)))

messageCallback :: Server -> Redis.Message -> IO PubSub
messageCallback server@Server{..} msg = 
  atomically $ do
    channel <- lookupChannel server (ChannelSlug $ decodeUtf8 $ msgChannel msg)
    case channel of
      Just chan -> do
        let broadcastMsg = Aeson.eitherDecode (LC8.fromStrict $ msgMessage msg) :: Either String RtmEvent
        case broadcastMsg of
          Right bMsg -> writeTChan (channelBroadcastChan chan) bMsg
          Left err   -> return ()
      Nothing -> return ()
    return mempty

broadcastEvent :: ChannelSlug -> RtmEvent -> RedisAction Integer
broadcastEvent chanId event = ExceptT $ ReaderT $ \conn -> runRedis conn $ publish (chanId2Bs chanId) (BL.toStrict $ Aeson.encode event)

lookupClient :: Server -> ClientId -> STM (Maybe Client)
lookupClient Server{..} name = M.lookup name <$> readTVar serverClients

chanId2Bs :: ChannelSlug -> BS.ByteString
chanId2Bs = encodeUtf8 . unSlug

lookupChannel :: Server -> ChannelSlug -> STM (Maybe Channel)
lookupChannel Server{..} name = M.lookup name <$> readTVar serverChannels

tryAddClient :: Server -> ClientId  -> IO (Maybe Client)
tryAddClient server@Server{..} name = atomically $ do
    clients <- readTVar serverClients
    if M.member name clients
        then return Nothing
        else do
            client <- newClient name
            writeTVar serverClients $ M.insert name client clients
            return (Just client)

chanAddClient :: JoinReason -> Channel -> ClientId -> STM ()
chanAddClient JoinReasonJoined    = chanAddClient' chanNotifyHasJoined
chanAddClient JoinReasonConnected = chanAddClient' chanNotifyHasConnected

chanAddClient' :: (Channel -> ClientId -> STM ()) -> Channel -> ClientId -> STM ()
chanAddClient' notifyAction chan@Channel{..} name = do
    notifyAction chan name
    modifyTVar channelClients . S.insert $ name

-- Notify the channel a client has left.
chanNotifyHasLeft :: Channel -> ClientId -> STM ()
chanNotifyHasLeft chan name = chanNotify chan RtmHello

-- Notify the channel a client has disconnected.
chanNotifyHasDisconnected :: Channel -> ClientId -> STM ()
chanNotifyHasDisconnected chan name = chanNotify chan RtmHello

-- Notify the channel a client has joined.
chanNotifyHasJoined :: Channel -> ClientId -> STM ()
chanNotifyHasJoined chan name = chanNotify chan RtmHello
