module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation)
import ClassyPrelude         as X
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Text.Shakespeare.Text (st)
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Yesod.Test            as X
import System.IO.Unsafe (unsafePerformIO)
import Database.Persist.Sql (fromSqlKey)
import qualified Data.ByteString.Lazy as BL
import Yesod.Core (Route)
import Network.HTTP.Types.Method

{-# NOINLINE unsafeApp #-}
unsafeApp :: IORef App
unsafeApp = unsafePerformIO (newIORef (unsafePerformIO initializeTestApp))

initializeTestApp :: IO App
initializeTestApp = do 
      settings <- loadAppSettings
          ["config/test-settings.yml", "config/settings.yml"]
          []
          ignoreEnv
      makeFoundation settings

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)


withApp :: SpecWith App -> Spec
withApp = before $ do
    -- settings <- loadAppSettings
    --     ["config/test-settings.yml", "config/settings.yml"]
    --     []
    --     ignoreEnv
    -- foundation <- makeFoundation settings
    foundation <- readIORef unsafeApp
    wipeDB foundation
    return foundation

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
    rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables
    
getWithAuth :: Route App -> YesodExample App ()        
getWithAuth resource = do 
  now <- liftIO getCurrentTime
  userId <- runDB $ insert (User 1 "LeviNotik" "some-url" Nothing now)
  request $ do 
    setMethod methodGet 
    setUrl resource
    addGetParam "dummy_auth" (pack . show . fromSqlKey $ userId)
  
postWithAuth :: Route App -> Maybe BL.ByteString -> YesodExample App ()
postWithAuth resource body = do 
  now <- liftIO getCurrentTime
  userId <- runDB $ insert (User 1 "LeviNotik" "some-url" Nothing now)
  request $ do 
    setMethod methodPost 
    setUrl resource
    maybe (return ()) setRequestBody body
    addGetParam "dummy_auth" (pack . show . fromSqlKey $ userId)
