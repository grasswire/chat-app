module Handler.ChatSpec (spec) where

import qualified Types as T

import TestImport
import Data.Aeson
import Network.HTTP.Types.Method

spec :: Spec
spec = withApp $ do
    it "loads the index and checks it looks right" $ do
        get HomeR
        statusIs 200
        htmlAnyContain "h1" "Chat about anything"
        htmlAnyContain "h1" "Create a room"
    
        -- request $ do
        --     setMethod "POST"
        --     setUrl HomeR
        --     addToken
        --     fileByLabel "Choose a file" "test/Spec.hs" "text/plain" -- talk about self-reference
        --     byLabel "What's on the file?" "Some Content"
        -- 
        -- statusIs 200
        -- more debugging printBody
        -- htmlCount ".message" 1
        -- htmlAllContain ".message" "Some Content"
        -- htmlAllContain ".message" "text/plain"

    -- This is a simple example of using a database access in a test.  The
    -- test will succeed for a fresh scaffolded site with an empty database,
    -- but will fail on an existing database with a non-empty user table.
    it "leaves the user table empty" $ do
        get HomeR
        statusIs 200
        users <- runDB $ selectList ([] :: [Filter User]) []
        assertEqual "user table empty" 0 $ length users

    it "returns a 401 if an unauthed user tries to create a new channel" $ do 
      postBody NewChatR (encode $ object ["title" .= ("title" :: Value), "topic" .= ("topic" :: Value), "color" .= ("color" :: Value)]) 
      statusIs 401
  
    it "returns a 201 if a user creates a channel" $ do 
      now <- liftIO getCurrentTime
      runDB $ insert (User 1 "LeviNotik" "some-url" Nothing now)
      request $ do 
        setMethod methodPost
        setUrl NewChatR
        setRequestBody (encode $ object ["title" .= ("title" :: Value), "topic" .= ("topic" :: Value), "color" .= ("color" :: Value)]) 
        addGetParam "dummy_auth" "1"
      
      statusIs 201
        
        
        
          -- twitterUserId Int64
          -- twitterScreenName Text
          -- profileImageUrl Text
          -- emailAddress Text Maybe
          -- lastSeen UTCTime default=current_timestamp