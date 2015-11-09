{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Handler.Home where

import Import
import Yesod.WebSockets


getHealthCheckR :: Handler Text
getHealthCheckR = return ("all good" :: Text)



chatApp :: WebSocketsT Handler ()
chatApp = do
    sendTextData ("Welcome to the chat server, please enter your name." :: Text)
    name <- receiveData
    sendTextData $ "Welcome, " <> name
    app <- getYesod
    inChan <- atomically $ do
        writeTChan (appWriteChan $ app) $ name <> " has joined the chat"
        dupTChan (appWriteChan $ app)
    race_
        (forever $ atomically (readTChan inChan) >>= sendTextData)
        (sourceWS $$ mapM_C (\msg ->
            atomically $ writeTChan (appWriteChan $ app) $ name <> ": " <> msg))

newtype RoomId = RoomId Integer

data ChatRoom = ChatRoom { title :: Text
                         , description :: Text}

getRoom :: RoomId -> IO (Maybe ChatRoom)
getRoom roomId = return (Just $ ChatRoom "scifi" "all things scifi")

getChatR :: Text -> Handler Html
getChatR roomId = do
    webSockets chatApp
    defaultLayout $ do
        $(widgetFile "chat-room")

-- getChatR :: Handler Html
-- getChatR = do
--     webSockets chatApp
--     defaultLayout $ do
--         [whamlet|
--
--
--                     <div #output>
--                     <form #form>
--                         <div #footer>
--                         <input #input autofocus>
--
--
--
--         |]
--         toWidget [julius|
--             var url = document.URL,
--                 output = document.getElementById("output"),
--                 form = document.getElementById("form"),
--                 input = document.getElementById("input"),
--                 conn;
--
--             url = url.replace("http:", "ws:").replace("https:", "wss:");
--
--             conn = new WebSocket(url);
--
--             conn.onmessage = function(e) {
--                 var p = document.createElement("p");
--                 p.appendChild(document.createTextNode(e.data));
--                 output.appendChild(p);
--             };
--
--             form.addEventListener("submit", function(e){
--                 conn.send(input.value);
--                 input.value = "";
--                 e.preventDefault();
--             });
--         |]
