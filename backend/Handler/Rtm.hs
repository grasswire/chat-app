{-# LANGUAGE TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Rtm where

import Import hiding ((==.), (>=.))

import           Data.Time.Clock
import           Taplike.Schema
import qualified Database.Esqueleto as E
import qualified Types as TP
import           Taplike.Shared (userFromEntity)
import           Database.Persist.Sql (fromSqlKey)
import           Types (RtmStartRp(..), Self(..))
import Queries 

getRtmStartR :: Handler Value
getRtmStartR = do 
  authId <- maybeAuthId
  user <- case authId of
            Just i -> fmap (Entity i) <$> runDB (get i)
            _      -> return Nothing       
  case user of 
    Nothing -> anonymousRtm 
    Just u  -> authenticatedRtm u 
  where anonymousRtm = do 
          mParam <- lookupGetParam "channel_slug"
          presenceF <- liftIO presenceFunc 
          case mParam of 
            Nothing -> sendResponseStatus badRequest400 ("BADREQUEST: anonymous user must provide a channel_slug param" :: Text)
            Just slug -> do 
              maybeChan <- runDB (getBy $ UniqueChannelSlug (ChannelSlug slug))
              case maybeChan of 
                Nothing -> sendResponseStatus status404 ("NOTFOUND: no channel found with the provided slug" :: Text)
                Just channel -> do 
                  members <- runDB (membersByChannel (entityKey channel))
                  users   <- runDB (usersByChannel (entityKey channel))
                  let response = RtmStartRp Nothing ((\u -> userFromEntity u (presenceF u)) <$> users) [chanFromEntity channel (TP.NumberUsersPresent 0) members] []
                  returnJson response  
        authenticatedRtm u = do 
          presenceF <- liftIO presenceFunc
          users      <- runDB (usersInUserChannels (entityKey u))
          myChannels <- runDB (usersChannelsWithMembers (entityKey u))
          messageHistory <- runDB (messageHistory 10 (entityKey . fst <$> myChannels))
          let groupedMessages = (map (\l@((_,t):_) -> (t, map fst l)) . groupBy ((==) `on` snd)) messageHistory :: [(Entity Channel, [Entity Message])]
              messages = (\(chan, msgs) -> TP.MessageHistory (TP.ChannelSlug $ unSlug $ channelCrSlug $ entityVal chan) (rtmMessageFromEntity chan <$> msgs)) <$> groupedMessages :: [TP.MessageHistory]
              results = second E.unValue <$> myChannels
              grouped = (map (\l@((h,_):_) -> (h, mapMaybe snd l)) . groupBy ((==) `on` fst)) results
              response = RtmStartRp (Just $ Self (TP.UserId $ fromSqlKey $ entityKey u) 
                                    (userTwitterScreenName $ entityVal u) (userProfileImageUrl $ entityVal u)) 
                                    ((\user -> userFromEntity user (presenceF user)) <$> users) (uncurry (flip chanFromEntity $ TP.NumberUsersPresent 0) <$> grouped) messages
          returnJson response
        presenceFunc = do 
          now <- getCurrentTime
          let func u = if diffUTCTime now (userLastSeen (entityVal u)) <= diffUTCTime now (addUTCTime (negate 1800 :: NominalDiffTime) now)
                       then TP.PresenceActive 
                       else TP.PresenceAway
          return func            
                     