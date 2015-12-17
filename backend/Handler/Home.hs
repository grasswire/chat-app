{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, TypeSynonymInstances, FlexibleContexts #-}

module Handler.Home where 
import           Import hiding (toLower)
import           Database.Persist.Sql (rawSql)
import           Taplike.Schema
import qualified Types as TP
import Model.Instances ()
import DataStore
import Data.Time.Clock
import qualified Database.Esqueleto   as E
import           Database.Esqueleto   ((==.), (^.), (>=.), (&&.), val)

getHomeR :: Handler Html
getHomeR = do
    authId <- maybeAuthId
    app <- getYesod
    let signature = "home" :: String
    let modalCreate = $(widgetFile "partials/modals/create")
    timeNow <- liftIO getCurrentTime
    (topChannels, allChannels) <- do
        chanEntities <- runDB (popularChannels' (addUTCTime (negate 3600 :: NominalDiffTime) timeNow))
        presences <- liftIO $ runRedisAction (redisConn app) $ 
                        getPresenceForChannels (channelCrSlug . entityVal <$> chanEntities)
        let zipped = case presences of
                      Right ps -> chanEntities `zip` ps
                      Left _   -> chanEntities `zip` replicate (length chanEntities) (TP.NumberUsersPresent 0)
        return $ splitAt 9 $ sortBy (flip compare `on` TP.channelNumUsersPresent ) $ uncurry chanFromEntity <$> zipped

    defaultLayout $ do
      setTitle "Taplike / Home"
      $(widgetFile "homepage")

popularChannelsStatement :: Text
popularChannelsStatement = "select ?? from channel where id in " <> 
                           "(select channel from message where timestamp >= ? group by channel order by count(*) desc limit 27);"

popularChannels :: MonadIO m => UTCTime -> ReaderT SqlBackend m [Entity Channel]
popularChannels since = rawSql popularChannelsStatement [PersistUTCTime since]

popularChannels' :: MonadIO m => UTCTime -> SqlPersistT m [Entity Channel]
popularChannels' ts = do 
  chans <- chansByMessages
  E.select $
    E.from $ \channel -> do 
    E.where_ (channel ^. ChannelId `E.in_` (E.valList $ E.unValue <$> chans))
    return channel  
  where chansByMessages = E.select $ 
                          E.from $ \message -> do 
                          E.where_ (message ^. MessageTimestamp E.>=. val ts)
                          E.groupBy (message ^. MessageChannel)
                          let cnt :: E.Esqueleto query expr backend => expr (E.Value Int)
                              cnt = E.countRows
                          E.orderBy [E.desc cnt]
                          E.limit 27
                          return (message ^. MessageChannel)  