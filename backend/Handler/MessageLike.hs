{-# LANGUAGE TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.MessageLike where 
  
import           Import 
import qualified Server as S
import qualified Types as TP
import Model.Instances ()
import Taplike.Schema
import DataStore
import Control.Concurrent (forkIO)
import           Database.Persist.Sql (fromSqlKey)

postMessageLikeR :: Handler ()
postMessageLikeR =  do
    messageLike <- requireJsonBody :: Handler TP.NewMessageLike 
    authId  <- maybeAuthId
    app <- getYesod
    case authId of
      Just liker -> do
        currentTime <- liftIO getCurrentTime
        key <- runDB (insert $ toPersist messageLike liker currentTime)
        runInnerHandler <- handlerToIO
        void $ liftIO $ forkIO $ runInnerHandler $ broadcastLikeAdded app liker messageLike
        sendResponseStatus status201 (toJSON TP.OkResponse)
      Nothing  -> sendResponseStatus status401 ("UNAUTHORIZED" :: Text)
    where 
        toPersist :: TP.NewMessageLike -> UserId -> UTCTime -> MessageLike 
        toPersist (TP.NewMessageLike (TP.MessageId msgId) (TP.ChannelSlug channelSlug)) userId =
          MessageLike (MessageUUID msgId) userId (ChannelSlug channelSlug)
        
        broadcastLikeAdded :: App -> UserId -> TP.NewMessageLike -> Handler ()
        broadcastLikeAdded (App { redisConn }) userId msgLike = do
          channel <- runDB (getBy $ UniqueChannelSlug (ChannelSlug $ TP.unChannelSlug $ TP.messageLikeChannel msgLike))
          case channel of 
            Just chan -> liftIO . void . runRedisAction redisConn $ 
              S.broadcastEvent (channelCrSlug $ entityVal chan) 
                             (TP.RtmMessageLikeAdded 
                               (TP.MessageLikeAdded (TP.UserId $ fromSqlKey userId) 
                               (TP.unMessageId $ TP.messageLikeMessageId msgLike)))
            Nothing   -> return ()




