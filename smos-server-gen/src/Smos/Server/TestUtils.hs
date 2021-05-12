{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.TestUtils where

import Control.DeepSeq
import Data.Pool
import Database.Persist.Sqlite as DB
import qualified Network.HTTP.Client as Http
import Servant.Auth.Client as Auth
import Servant.Auth.Server as Auth
import Smos.API.Gen ()
import Smos.Client
import Smos.Server.Handler.Import as Server
import Smos.Server.Serve as Server
import Test.Syd
import Test.Syd.Persistent.Sqlite
import Test.Syd.Wai
import UnliftIO

data ServerTestEnv = ServerTestEnv
  { serverTestEnvPool :: Pool SqlBackend,
    serverTestEnvClient :: ClientEnv
  }

type ServerTestEnvM a = ReaderT ServerTestEnv IO a

serverEnvSpec :: TestDef '[Http.Manager] ServerTestEnv -> Spec
serverEnvSpec = modifyMaxSuccess (`div` 20) . managerSpec . setupAroundWith' serverTestEnvSetupFunc

serverTestEnvSetupFunc :: Http.Manager -> SetupFunc () ServerTestEnv
serverTestEnvSetupFunc man = do
  pool <- serverConnectionPoolSetupFunc
  cenv <- unwrapSetupFunc (serverSetupFunc' man) pool
  pure $ ServerTestEnv {serverTestEnvPool = pool, serverTestEnvClient = cenv}

serverEnvDB :: SqlPersistT IO a -> ServerTestEnvM a
serverEnvDB func = do
  pool <- asks serverTestEnvPool
  liftIO $ DB.runSqlPool func pool

serverEnvClient :: NFData a => ClientM a -> ServerTestEnvM (Either ClientError a)
serverEnvClient func = do
  cenv <- asks serverTestEnvClient
  liftIO $ runClient cenv func

serverEnvClientOrErr :: NFData a => ClientM a -> ServerTestEnvM a
serverEnvClientOrErr func = do
  cenv <- asks serverTestEnvClient
  liftIO $ testClient cenv func

withServerEnvNewUser :: (Token -> ServerTestEnvM ()) -> ServerTestEnvM ()
withServerEnvNewUser func = do
  cenv <- asks serverTestEnvClient
  withNewUser cenv func

serverDBSpec :: SpecWith ConnectionPool -> Spec
serverDBSpec = modifyMaxSuccess (`div` 10) . setupAround serverConnectionPoolSetupFunc

serverConnectionPoolSetupFunc :: SetupFunc () ConnectionPool
serverConnectionPoolSetupFunc = connectionPoolSetupFunc serverAutoMigration

type ServerSpec = TestDef '[Http.Manager] ClientEnv

serverSpec :: ServerSpec -> Spec
serverSpec = modifyMaxSuccess (`div` 20) . managerSpec . setupAroundWith' serverSetupFunc

serverSetupFunc :: Http.Manager -> SetupFunc () ClientEnv
serverSetupFunc man = serverConnectionPoolSetupFunc `connectSetupFunc` serverSetupFunc' man

serverSetupFunc' :: Http.Manager -> SetupFunc ConnectionPool ClientEnv
serverSetupFunc' man = wrapSetupFunc $ \pool -> do
  application <- liftIO $ do
    uuid <- nextRandomUUID
    jwtKey <- Auth.generateKey
    -- We turn logging off while tests pass, but for debugging the logs will
    -- probably be useful.  So while debugging you may want to uncomment the
    -- next line and comment out the one after that.
    --
    -- logFunc <- runStderrLoggingT askLoggerIO
    logFunc <- runNoLoggingT askLoggerIO
    let env =
          ServerEnv
            { serverEnvServerUUID = uuid,
              serverEnvConnection = pool,
              serverEnvCookieSettings = defaultCookieSettings,
              serverEnvJWTSettings = defaultJWTSettings jwtKey,
              serverEnvPasswordDifficulty = 4, -- The lowest (fastest)
              serverEnvCompressionLevel = 1, -- The lowest (fastest)
              serverEnvLogFunc = logFunc,
              serverEnvMaxBackupsPerUser = Nothing,
              serverEnvMaxBackupSizePerUser = Nothing,
              serverEnvAdmin = Nothing
            }
    pure $ Server.makeSyncApp env
  p <- unwrapSetupFunc applicationSetupFunc application
  -- The fromIntegral is safe because it's PortNumber -> Int
  pure $ mkClientEnv man (BaseUrl Http "127.0.0.1" (fromIntegral p) "")

registerLogin :: Register -> Login
registerLogin register =
  Login {loginUsername = registerUsername register, loginPassword = registerPassword register}

testLogin :: ClientEnv -> Login -> IO Token
testLogin cenv lf = do
  errOrRes <- login cenv lf
  case errOrRes of
    Left err -> expectationFailure $ "Failed to login: " <> show err
    Right t -> pure t

withNewUser :: MonadUnliftIO m => ClientEnv -> (Token -> m ()) -> m ()
withNewUser cenv func = withNewUserAndData cenv $ const func

withNewUserAndData :: MonadUnliftIO m => ClientEnv -> (Register -> Token -> m a) -> m a
withNewUserAndData cenv func = do
  r <- liftIO randomRegistration
  withNewGivenUser cenv r $ func r

withNewGivenUser :: MonadIO m => ClientEnv -> Register -> (Token -> m a) -> m a
withNewGivenUser cenv r func = do
  t <-
    liftIO $ do
      NoContent <- testClient cenv $ clientPostRegister r
      testLogin cenv (registerLogin r)
  func t

withNewRegisteredUser :: MonadIO m => ClientEnv -> (Register -> m a) -> m a
withNewRegisteredUser cenv func = do
  r <- liftIO randomRegistration
  NoContent <- liftIO $ testClient cenv $ clientPostRegister r
  func r

randomRegistration :: IO Register
randomRegistration = do
  u1 <- nextRandomUUID :: IO (UUID Username) -- Dummy's that are significantly likely to be random enough
  u2 <- nextRandomUUID :: IO (UUID Password)
  un <- parseUsername $ uuidText u1
  let pw = uuidText u2
  pure Register {registerUsername = un, registerPassword = pw}

testClient :: NFData a => ClientEnv -> ClientM a -> IO a
testClient cenv func = do
  errOrRes <- runClient cenv func
  case errOrRes of
    Left err -> expectationFailure $ show err
    Right r -> pure r
