{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Yesod.WebSockets


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



getChatR :: Handler Html
getChatR = do
    webSockets chatApp
    defaultLayout $ do
        [whamlet|
            <div #output>
            <form #form>
                <input #input autofocus>
        |]
        toWidget [lucius|
            \#output {
                width: 600px;
                height: 400px;
                border: 1px solid black;
                margin-bottom: 1em;
                p {
                    margin: 0 0 0.5em 0;
                    padding: 0 0 0.5em 0;
                    border-bottom: 1px dashed #99aa99;
                }
            }
            \#input {
                width: 900px;
                display: block;
            }
        |]
        toWidget [julius|
            var url = document.URL,
                output = document.getElementById("output"),
                form = document.getElementById("form"),
                input = document.getElementById("input"),
                conn;

            url = url.replace("http:", "ws:").replace("https:", "wss:");

            conn = new WebSocket(url);

            conn.onmessage = function(e) {
                var p = document.createElement("p");
                p.appendChild(document.createTextNode(e.data));
                output.appendChild(p);
            };

            form.addEventListener("submit", function(e){
                conn.send(input.value);
                input.value = "";
                e.preventDefault();
            });
        |]

sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing
