{-# LANGUAGE OverloadedStrings #-}

module Handler.Channel where 
  
import Import 
import Database.Persist.Sql (rawSql)
import Data.Time.Clock
import Control.Arrow ((***))
import Taplike.Schema
import Taplike.Shared (messageFromEntity)
import qualified Types as TP

  
getChannelLikesR :: Handler Value
getChannelLikesR = do 
  let chanParam = "channel"
  app <- getYesod 
  channel <- lookupGetParam chanParam
  case channel of 
    Just slug -> do 
      queryResult <- runDB (mostLikedMessages $ ChannelSlug slug)
      let vals =  (entityVal *** entityVal) <$> queryResult
      let grouped = (map (\l@((_,t):_) -> (t, map fst l)) . groupBy ((==) `on` snd)) vals :: [(Message, [MessageLike])]
      let messages = (\(msg, likes) -> messageFromEntity msg (TP.ChannelSlug slug) likes) <$> grouped
      returnJson messages
    Nothing -> sendResponseStatus badRequest400 ("BADREQUEST: MISSING " <> chanParam <> " param" :: Text)
    
mostLikedMessagesStatement :: Text
mostLikedMessagesStatement = "select ??, ?? from message_like inner join message on " <> 
                              "(message_like.message = message.uuid) where message in " <> 
                              "(select message from message_like where timestamp >= ? " <>
                              "and channel = ? group by message order by count(*) desc limit 10)"

dayAgo :: IO UTCTime 
dayAgo = addUTCTime (negate 86400 :: NominalDiffTime) <$> getCurrentTime 

mostLikedMessages :: MonadIO m => ChannelSlug -> ReaderT SqlBackend m [(Entity MessageLike, Entity Message)]
mostLikedMessages slug = do 
  since <- liftIO dayAgo
  rawSql mostLikedMessagesStatement [PersistUTCTime since, toPersistValue slug]