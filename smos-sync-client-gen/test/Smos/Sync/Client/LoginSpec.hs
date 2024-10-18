module Smos.Sync.Client.LoginSpec
  ( spec,
  )
where

import Control.Concurrent
import Control.Monad.Logger
import Crypto.JOSE as JOSE
import Path.IO
import Servant.Auth.Server as Auth
import Servant.Client
import Smos.API.Gen ()
import Smos.Server.Handler.Import as Server
import Smos.Server.Serve as Server
import Smos.Sync.Client.Command.Login
import Smos.Sync.Client.Command.Register
import Smos.Sync.Client.Command.Sync
import Smos.Sync.Client.OptParse as Client
import Test.Syd
import Test.Syd.Persistent.Sqlite (withConnectionPool)
import Test.Syd.Validity
import Test.Syd.Wai

spec :: Spec
spec = managerSpec $
  modifyMaxSuccess (`div` 20) $ do
    itWithOuter "can still login nicely if the server's JWT key changes" $ \man ->
      forAllValid $ \username ->
        forAllValid $ \password ->
          withConnectionPool serverAutoMigration $ \pool -> do
            uuid <- nextRandomUUID

            let serverSetupFuncWithJWTKey :: JOSE.JWK -> SetupFunc ClientEnv
                serverSetupFuncWithJWTKey key = do
                  priceVar <- liftIO newEmptyMVar
                  let env =
                        ServerEnv
                          { serverEnvServerUUID = uuid,
                            serverEnvConnection = pool,
                            serverEnvCookieSettings = defaultCookieSettings,
                            serverEnvJWTSettings = defaultJWTSettings key,
                            serverEnvPasswordDifficulty = 4, -- The lowest
                            serverEnvCompressionLevel = 1, -- The lowest
                            serverEnvLogFunc = \_ _ _ _ -> pure (),
                            serverEnvMaxBackupSizePerUser = Nothing,
                            serverEnvAdmin = Nothing,
                            serverEnvBookingEmailAddress = Nothing,
                            serverEnvPriceCache = priceVar,
                            serverEnvMonetisationSettings = Nothing
                          }
                  let application = Server.makeSyncApp env
                  p <- applicationSetupFunc application
                  -- The fromIntegral is safe because it's PortNumber -> Int
                  pure $ mkClientEnv man (BaseUrl Http "127.0.0.1" (fromIntegral p) "")
            let withServerWithKey key func = unSetupFunc (serverSetupFuncWithJWTKey key) func

            withSystemTempDir "smos-sync-client-test-login" $ \tdir -> do
              sessionFile <- resolveFile tdir "session.dat"
              uuidFile <- resolveFile tdir "server-uuid.json"
              contentsDir <- resolveDir tdir "contents"
              metadataDBFile <- resolveFile tdir "metadata.sqlite3"
              backupDir <- resolveDir tdir "conflict-backups"

              let setsFromCenv cenv = do
                    Client.Settings
                      { setServerUrl = baseUrl cenv,
                        setLogLevel = LevelError,
                        setUsername = Just username,
                        setPassword = Just password,
                        setSessionPath = sessionFile -- the same session file path!
                      }
              let syncSets =
                    SyncSettings
                      { syncSetContentsDir = contentsDir,
                        syncSetUUIDFile = uuidFile,
                        syncSetMetadataDB = metadataDBFile,
                        syncSetBackupDir = backupDir,
                        syncSetIgnoreFiles = IgnoreHiddenFiles,
                        syncSetEmptyDirs = RemoveEmptyDirs
                      }
              -- Use one JWK to authenticate the first time
              jwtKey1 <- Auth.generateKey
              withServerWithKey jwtKey1 $ \cenv -> do
                let settings = setsFromCenv cenv
                registerSmosSyncClient settings
                loginSmosSyncClient settings
                syncSmosSyncClient settings syncSets
              -- Then switch the JWK to make authentication fail, to test the retry logic
              jwtKey2 <- Auth.generateKey
              withServerWithKey jwtKey2 $ \cenv -> do
                let settings = setsFromCenv cenv
                syncSmosSyncClient settings syncSets
