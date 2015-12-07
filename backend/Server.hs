{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Server where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           System.IO
import           Prelude                  ((.), ($), (++), show, putStrLn, Integer, read)
import           Data.Int                 (Int64)
import           Data.String
import qualified Types                    as TP
import           Data.Maybe
import qualified Data.ByteString          as BS
import qualified Data.Text.Lazy           as T
import qualified Data.ByteString.Lazy     as BL
import           Data.Text.Lazy           (Text)
import           Taplike.Shared           (RtmEvent(..))
import           Model                    (ChannelId(..), Message(..))
import           Database.Redis hiding    (Message)
import qualified Database.Redis as Redis
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.Monoid              ((<>), mempty)
import           DataStore
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import qualified Data.Aeson as Aeson
import           Database.Persist.Sql     (fromSqlKey)
import           Debug.Trace
import           Control.Concurrent       (forkIO)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)

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
    { channelId            :: ChannelId
    , channelClients       :: TVar (S.Set ClientId)
    , channelBroadcastChan :: TChan RtmEvent
    }

newChannel :: ChannelId -> STM Channel
newChannel name = Channel name <$> newTVar S.empty <*> newBroadcastTChan

-- Send a Notice to the channel.
chanNotify :: Channel -> RtmEvent -> STM ()
chanNotify chan event = chanMessage chan event

-- Send a Message to the channel.
chanMessage :: Channel -> RtmEvent -> STM ()
chanMessage = writeTChan . channelBroadcastChan

-- Notify the channel a client has connected.
chanNotifyHasConnected :: Channel -> ClientId -> STM ()
chanNotifyHasConnected chan name = chanNotify chan RtmHello

data Server = Server
    { serverChannels       :: TVar (Map ChannelId Channel)
    , serverClients        :: TVar (Map ClientId Client)
    , serverSubscriptions  :: TVar PubSub
    }

newServer :: IO Server
newServer = atomically $ do
    server <- Server <$> newTVar M.empty <*> newTVar M.empty <*> newTVar mempty
    return server

lookupOrCreateChannel :: Connection -> Server -> ChannelId -> IO Channel
lookupOrCreateChannel conn server@Server{..} name = do
      channel <- atomically $ lookupChannel server name
      let pubSubChan = chanId2Bs name
      case channel of
        Nothing -> do
          (subs, newChan) <- atomically $ do
              chan <- newChannel name
              modifyTVar serverChannels . M.insert name $ chan
              modifyTVar serverSubscriptions ((subscribe [pubSubChan]) <>)
              currentSubscriptions <- readTVar serverSubscriptions
              return (currentSubscriptions, chan)
          forkIO $ runRedis conn (pubSub subs (messageCallback server))
          return newChan
        Just chan -> return chan

messageCallback :: Server -> (Redis.Message -> IO PubSub)
messageCallback server@Server{..} = \msg -> do
  atomically $ do
    channel <- lookupChannel server (toSqlKey (read $ C8.unpack $ msgChannel msg :: Int64))
    case channel of
      Just chan -> do
        let broadcastMsg = Aeson.decode (LC8.fromStrict $ msgMessage msg) :: Maybe RtmEvent
        case broadcastMsg of
          Just bMsg -> writeTChan (channelBroadcastChan chan) bMsg
          Nothing   -> return ()
  return mempty

broadcastEvent :: ChannelId -> RtmEvent -> RedisAction Integer
broadcastEvent chanId event = ExceptT $ ReaderT $ \conn -> runRedis conn $ publish (chanId2Bs chanId) (BL.toStrict $ Aeson.encode event)

lookupClient :: Server -> ClientId -> STM (Maybe Client)
lookupClient Server{..} name = M.lookup name <$> readTVar serverClients

chanId2Bs :: ChannelId -> BS.ByteString
chanId2Bs = C8.pack . show .fromSqlKey

lookupChannel :: Server -> ChannelId -> STM (Maybe Channel)
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

listUsers :: Channel -> STM [Int64]
listUsers channel = S.toList <$> (readTVar $ channelClients channel)

-- Notify the channel a client has left.
chanNotifyHasLeft :: Channel -> ClientId -> STM ()
chanNotifyHasLeft chan name = chanNotify chan RtmHello

-- Notify the channel a client has disconnected.
chanNotifyHasDisconnected :: Channel -> ClientId -> STM ()
chanNotifyHasDisconnected chan name = chanNotify chan RtmHello

-- Notify the channel a client has joined.
chanNotifyHasJoined :: Channel -> ClientId -> STM ()
chanNotifyHasJoined chan name = chanNotify chan RtmHello
