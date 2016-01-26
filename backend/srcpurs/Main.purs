module Main where

import Prelude
import Control.Monad.Eff.Console
import qualified Control.Monad.Eff.JQuery as JQuery
import Network.HTTP.Affjax
import Network.HTTP.Method
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import qualified WebSocket as WS
import WebSocket hiding (URL(..)) -- (newWebSocket, onopen, send, onmessage, runMessageEvent, runMessage)
import Control.Bind ((=<<))
import Control.Monad.Eff.Var (($=))
import qualified Control.Monad.Eff.Var as EffVar
import Control.Monad.Eff.Console.Unsafe (logAny)
import Control.Monad (when)
import Data.Maybe (Maybe(..))
import Data.String (replace)

foreign import rtmStartUrl :: String
foreign import currentChannel :: String
foreign import websocketUrl :: String

main = JQuery.ready $ do
  launchAff $ do
    res <- affjax $ defaultRequest { url = rtmStartUrl, method = GET }
    liftEff $ log $ "GET /api response: " ++ res.response
    liftEff $ log websocketUrl
  
  Connection socket <- newWebSocket (WS.URL $ replace "http" "ws" websocketUrl) []

  socket.onopen $= \event -> do
    logAny event
    log "onopen: Connection opened"

    log <<< runURL =<< EffVar.get socket.url

    log "onopen: Sending 'hello'"
    -- socket.send (Message "hello")

    log "onopen: Sending 'goodbye'"
    -- socket.send (Message "goodbye")

  socket.onmessage $= \event -> do
    logAny event
    let received = runMessage (runMessageEvent event)

    log $ "onmessage: Received '" ++ received ++ "'"

    when (received == "goodbye") do
      log "onmessage: closing connection"
      socket.close Nothing Nothing

  socket.onclose $= \event -> do
    logAny event
    log "onclose: Connection closed"
  return unit