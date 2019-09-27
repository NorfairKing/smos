module Smos.Sync.Server.TestUtils where

import Data.Mergeful
import Data.UUID.V4 as UUID

import Path.IO

import Control.Concurrent.STM

import Servant.Client

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
    storeFile <- resolveFile tmpDir "store.json"
    let mkApp = do
          uuid <- UUID.nextRandom
          storeVar <- newTVarIO initialServerStore
          pure $
            Server.makeSyncApp
              ServerEnv
                { serverEnvServerUUID = uuid
                , serverEnvStoreFile = storeFile
                , serverEnvStoreVar = storeVar
                }
    Warp.testWithApplication mkApp $ \p ->
      let cenv = mkClientEnv man (BaseUrl Http "127.0.0.1" p "")
       in func cenv
