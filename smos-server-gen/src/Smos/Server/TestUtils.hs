{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.TestUtils where

import Data.Pool

import Control.Monad.IO.Class

import Control.Monad
import Control.Monad.Logger

import Servant.Auth.Client as Auth
import Servant.Auth.Server as Auth
import Servant.Client

import Database.Persist.Sqlite as DB

import qualified Network.HTTP.Client as Http
import Network.Wai.Handler.Warp as Warp (testWithApplication)

import Test.Hspec
import Test.Hspec.Core.QuickCheck

import Smos.API.Gen ()
import Smos.Client

import Smos.Server.Handler.Import as Server
import Smos.Server.Serve as Server

dbSpec :: SpecWith (Pool SqlBackend) -> Spec
dbSpec = modifyMaxShrinks (const 0) . modifyMaxSuccess (`div` 10) . around withDB

withDB :: (Pool SqlBackend -> IO a) -> IO a
withDB func =
  runNoLoggingT $
  DB.withSqlitePool ":memory:" 1 $ \pool -> do
    DB.runSqlPool (void $ DB.runMigrationSilent migrateAll) pool
    liftIO $ func pool

serverSpec :: SpecWith ClientEnv -> Spec
serverSpec = modifyMaxShrinks (const 0) . modifyMaxSuccess (`div` 20) . around withTestServer

withTestServer :: (ClientEnv -> IO a) -> IO a
withTestServer func = do
  man <- Http.newManager Http.defaultManagerSettings
  runNoLoggingT $
    DB.withSqlitePool ":memory:" 1 $ \pool ->
      liftIO $ do
        let mkApp = do
              uuid <- nextRandomUUID
              flip DB.runSqlPool pool $ void $ DB.runMigrationSilent migrateAll
              jwtKey <- Auth.generateKey
              let env =
                    ServerEnv
                      { serverEnvServerUUID = uuid
                      , serverEnvConnection = pool
                      , serverEnvCookieSettings = defaultCookieSettings
                      , serverEnvJWTSettings = defaultJWTSettings jwtKey
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
    Left err -> do
      expectationFailure $ show err
      undefined
    Right r -> pure r

registerLogin :: Register -> Login
registerLogin register =
  Login {loginUsername = registerUsername register, loginPassword = registerPassword register}

testLogin :: ClientEnv -> Login -> IO Token
testLogin cenv lf = do
  errOrRes <- login cenv lf
  case errOrRes of
    Left err -> failure $ "Failed to login: " <> show err
    Right t -> pure t

failure :: String -> IO a
failure s = do
  expectationFailure s
  error "Won't get here anyway"

withNewUser :: ClientEnv -> (Token -> IO ()) -> Expectation
withNewUser cenv func = withNewUserAndData cenv $ const func

withNewUserAndData :: ClientEnv -> (Register -> Token -> IO a) -> IO a
withNewUserAndData cenv func = do
  r <- randomRegistration
  withNewGivenUser cenv r $ func r

randomRegistration :: IO Register
randomRegistration = do
  u1 <- nextRandomUUID :: IO (UUID Username) -- Dummy's that are significantly likely to be random enough
  u2 <- nextRandomUUID :: IO (UUID Password)
  un <- parseUsername $ uuidText u1
  pw <- parsePassword $ uuidText u2
  pure Register {registerUsername = un, registerPassword = pw}

withNewGivenUser :: MonadIO m => ClientEnv -> Register -> (Token -> m a) -> m a
withNewGivenUser cenv r func = do
  t <-
    liftIO $ do
      NoContent <- testClientOrErr cenv $ clientPostRegister r
      testLogin cenv (registerLogin r)
  func t
