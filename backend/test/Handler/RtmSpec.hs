module Handler.RtmSpec (spec) where

import TestImport
import Network.HTTP.Types.Method
import Database.Persist.Sql (fromSqlKey)

spec :: Spec
spec = withApp $ do
    describe "rtm.start" $ do
      describe "when not authenticated" $ do  
        it "returns 401 if channel_slug param is missing" $ do 
          get RtmStartR
          statusIs 400
        it "returns a 404 if the supplied channel doesn't exist" $ do 
          request $ do 
            setMethod methodGet 
            setUrl RtmStartR
            addGetParam "channel_slug" (pack $ "foobar")
          statusIs 404
      describe "when authenticated" $ do 
        it "returns a 200" $ do 
          now <- liftIO getCurrentTime
          userId <- runDB $ insert (User 1 "LeviNotik" "some-url" Nothing now)
          request $ do 
            setMethod methodGet
            setUrl RtmStartR
            addGetParam "dummy_auth" (pack . show . fromSqlKey $ userId)
          
          statusIs 200

          
    -- describe "valid requests" $ do   
    --   it "returns a 201 if a user creates a channel" $ do 
    --     now <- liftIO getCurrentTime
    --     userId <- runDB $ insert (User 1 "LeviNotik" "some-url" Nothing now)
    --     request $ do 
    --       setMethod methodPost
    --       setUrl NewChatR
    --       setRequestBody channelPostBody
    --       addGetParam "dummy_auth" (pack . show . fromSqlKey $ userId)
    --     
    --     statusIs 201