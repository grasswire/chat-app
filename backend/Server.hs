{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Server where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           System.IO
import           Prelude                  ((.), ($))
import Data.Int (Int64)
import Data.String
import Types
import Data.Maybe
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import Taplike.Shared (RtmEvent(..))

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
    { channelName          :: ChannelName
    , channelClients       :: TVar (S.Set ClientId)
    , channelBroadcastChan :: TChan RtmEvent
    }

newChannel :: ChannelName -> STM Channel
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
    { serverChannels :: TVar (Map ChannelName Channel)
    , serverClients  :: TVar (Map ClientId Client)
    }

lookupClient :: Server -> ClientId -> STM (Maybe Client)
lookupClient Server{..} name = M.lookup name <$> readTVar serverClients

lookupOrCreateChannel :: Server -> ChannelName -> STM Channel
lookupOrCreateChannel server@Server{..} name = lookupChannel server name >>= \case
    Nothing -> do
        chan <- newChannel name
        modifyTVar serverChannels . M.insert name $ chan
        return chan
    Just chan -> return chan

    -- Look up a channel on the server, by name.
lookupChannel :: Server -> ChannelName -> STM (Maybe Channel)
lookupChannel Server{..} name = M.lookup name <$> readTVar serverChannels

newServer :: IO Server
newServer = atomically $ do
    server <- Server <$> newTVar M.empty <*> newTVar M.empty
    return server

addChannel :: Server -> ChannelName -> STM ()
addChannel Server{..} name = newChannel name >>= modifyTVar serverChannels . M.insert name

-- Try to add a client to the server; fail if the requested name is taken.
tryAddClient :: Server -> ClientId  -> IO (Maybe Client)
tryAddClient server@Server{..} name = atomically $ do
    clients <- readTVar serverClients
    if M.member name clients
        then return Nothing
        else do
            client <- newClient name
            -- notify client welcomeMsg
            writeTVar serverClients $ M.insert name client clients
            return (Just client)

welcomeMsg :: Text
welcomeMsg = T.unlines
    [ "Welcome to the server! Available commands:"
    , "/whisper name msg - whisper 'msg' to 'name'"
    , "/join    name     - join channel 'name'"
    , "/users            - list the users in the current channel"
    , "/whoami           - list your name and channel"
    , "/kick    name     - kick 'name'"
    , "/quit             - quit the server"
    ]

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
