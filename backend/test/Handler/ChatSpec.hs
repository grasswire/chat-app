module Handler.ChatSpec (spec) where

import TestImport
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Taplike.Schema 

channelPostBody :: BL.ByteString
channelPostBody = encode $ object ["title" .= ("title" :: Value), "topic" .= ("topic" :: Value), "color" .= ("color" :: Value)]

spec :: Spec
spec = withApp $ do
  describe "creating a channel" $ do
    describe "when a user is not authenticated" $ do 
      it "returns a 401" $ do 
        postBody NewChatR channelPostBody
        statusIs 401
    describe "when a user is authenticated" $ do   
      it "returns a 201" $ do 
        postWithAuth NewChatR (Just channelPostBody)
        statusIs 201
    describe "getChatR" $ do 
      it "returns a 200" $ do 
        getWithAuth (ChatR (ChannelSlug $ pack "some channel"))
        statusIs 200
