{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.TestUtils where

import Control.Monad.IO.Class
import Data.Pool
import Database.Persist.Sqlite as DB
import qualified Network.HTTP.Client as Http
import Network.Wai.Handler.Warp as Warp (testWithApplication)
import Servant.Auth.Client as Auth
import Servant.Auth.Server as Auth
import Servant.Client
import Smos.API.Gen ()
import Smos.Client
import Smos.Server.Handler.Import as Server
import Smos.Server.Serve as Server
import Test.Syd
import Test.Syd.Persistent.Sqlite

data ServerTestEnv = ServerTestEnv
  { serverTestEnvPool :: Pool SqlBackend,
    serverTestEnvClient :: ClientEnv
  }

serverEnvSpec :: TestDef '[Http.Manager] ServerTestEnv -> Spec
serverEnvSpec = modifyMaxSuccess (`div` 20) . beforeAll (Http.newManager Http.defaultManagerSettings) . setupAroundWith' serverTestEnvSetupFunc

serverTestEnvSetupFunc :: Http.Manager -> SetupFunc () ServerTestEnv
serverTestEnvSetupFunc man = do
  pool <- serverConnectionPoolSetupFunc
  cenv <- unwrapSetupFunc (clientEnvSetupFunc' man) pool
  makeSimpleSetupFunc $ \func ->
    let ste = ServerTestEnv {serverTestEnvPool = pool, serverTestEnvClient = cenv}
     in func ste

serverEnvDB :: ServerTestEnv -> SqlPersistT IO a -> IO a
serverEnvDB ServerTestEnv {..} func = DB.runSqlPool func serverTestEnvPool

serverEnvClient :: ServerTestEnv -> ClientM a -> IO (Either ClientError a)
serverEnvClient senv = testClient (serverTestEnvClient senv)

serverEnvClientOrErr :: ServerTestEnv -> ClientM a -> IO a
serverEnvClientOrErr senv = testClientOrErr (serverTestEnvClient senv)

withServerEnvNewUser :: ServerTestEnv -> (Token -> IO ()) -> Expectation
withServerEnvNewUser senv = withNewUser (serverTestEnvClient senv)

serverDBSpec :: SpecWith ConnectionPool -> Spec
serverDBSpec = modifyMaxSuccess (`div` 10) . setupAround serverConnectionPoolSetupFunc

serverConnectionPoolSetupFunc :: SetupFunc () ConnectionPool
serverConnectionPoolSetupFunc = connectionPoolSetupFunc migrateAll

type ServerSpec = TestDef '[Http.Manager] ClientEnv

serverSpec :: ServerSpec -> Spec
serverSpec = modifyMaxSuccess (`div` 20) . beforeAll (Http.newManager Http.defaultManagerSettings) . setupAroundWith' clientEnvSetupFunc

clientEnvSetupFunc :: Http.Manager -> SetupFunc () ClientEnv
clientEnvSetupFunc man = serverConnectionPoolSetupFunc `connectSetupFunc` clientEnvSetupFunc' man

clientEnvSetupFunc' :: Http.Manager -> SetupFunc ConnectionPool ClientEnv
clientEnvSetupFunc' man = SetupFunc $ \func pool -> do
  liftIO $ do
    let mkApp = do
          uuid <- nextRandomUUID
          jwtKey <- Auth.generateKey
          let env =
                ServerEnv
                  { serverEnvServerUUID = uuid,
                    serverEnvConnection = pool,
                    serverEnvCookieSettings = defaultCookieSettings,
                    serverEnvJWTSettings = defaultJWTSettings jwtKey,
                    serverEnvPasswordDifficulty = 4 -- The lowest
                  }
          pure $ Server.makeSyncApp env
    Warp.testWithApplication mkApp $ \p ->
      let cenv = mkClientEnv man (BaseUrl Http "127.0.0.1" p "")
       in func cenv

testClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
testClient = flip runClientM

testClientOrErr :: ClientEnv -> ClientM a -> IO a
testClientOrErr cenv func = do
  res <- testClient cenv func
  case res of
    Left err -> expectationFailure $ show err
    Right r -> pure r

registerLogin :: Register -> Login
registerLogin register =
  Login {loginUsername = registerUsername register, loginPassword = registerPassword register}

testLogin :: ClientEnv -> Login -> IO Token
testLogin cenv lf = do
  errOrRes <- login cenv lf
  case errOrRes of
    Left err -> expectationFailure $ "Failed to login: " <> show err
    Right t -> pure t

withNewUser :: ClientEnv -> (Token -> IO ()) -> Expectation
withNewUser cenv func = withNewUserAndData cenv $ const func

withNewUserAndData :: ClientEnv -> (Register -> Token -> IO a) -> IO a
withNewUserAndData cenv func = do
  r <- randomRegistration
  withNewGivenUser cenv r $ func r

withNewGivenUser :: MonadIO m => ClientEnv -> Register -> (Token -> m a) -> m a
withNewGivenUser cenv r func = do
  t <-
    liftIO $ do
      NoContent <- testClientOrErr cenv $ clientPostRegister r
      testLogin cenv (registerLogin r)
  func t

withNewRegisteredUser :: MonadIO m => ClientEnv -> (Register -> m a) -> m a
withNewRegisteredUser cenv func = do
  r <- liftIO randomRegistration
  NoContent <- liftIO $ testClientOrErr cenv $ clientPostRegister r
  func r

randomRegistration :: IO Register
randomRegistration = do
  u1 <- nextRandomUUID :: IO (UUID Username) -- Dummy's that are significantly likely to be random enough
  u2 <- nextRandomUUID :: IO (UUID Password)
  un <- parseUsername $ uuidText u1
  let pw = uuidText u2
  pure Register {registerUsername = un, registerPassword = pw}
