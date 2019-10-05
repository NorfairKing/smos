module Smos.Sync.Server.TestUtils where

import Data.Mergeful
import Data.Text as T
import Data.UUID.V4 as UUID

import Control.Monad.IO.Class

import Path
import Path.IO

import Control.Concurrent.MVar
import Control.Monad.Logger

import Servant.Client

import Database.Persist.Sqlite as DB

import qualified Network.HTTP.Client as Http
import Network.Wai.Handler.Warp as Warp (testWithApplication)

import Test.Hspec
import Test.Hspec.Core.QuickCheck

import Smos.Sync.Server.Handler.Import as Server
import Smos.Sync.Server.Serve as Server

serverSpec :: SpecWith ClientEnv -> Spec
serverSpec = modifyMaxShrinks (const 0) . modifyMaxSuccess (`div` 20) . around withTestServer

withTestServer :: (ClientEnv -> IO a) -> IO a
withTestServer func = do
  man <- Http.newManager Http.defaultManagerSettings
  withSystemTempDir "smos-sync-server-test" $ \tmpDir -> do
    dbFile <- resolveFile tmpDir "database.sqlite3"
    runNoLoggingT $
      DB.withSqlitePool (T.pack $ fromAbsFile dbFile) 1 $ \pool ->
        liftIO $ do
          let mkApp = do
                uuid <- UUID.nextRandom
                cacheVar <- newMVar initialServerStore
                let env =
                      ServerEnv
                        { serverEnvServerUUID = uuid
                        , serverEnvStoreCache = cacheVar
                        , serverEnvConnection = pool
                        }
                pure $ Server.makeSyncApp env
          Warp.testWithApplication mkApp $ \p ->
            let cenv = mkClientEnv man (BaseUrl Http "127.0.0.1" p "")
             in func cenv
