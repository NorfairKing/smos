{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.TestUtils where

import Control.Monad.IO.Class

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Logger

import Servant.Auth.Server as Auth
import Servant.Auth.Client as Auth
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
              store <-
                flip DB.runSqlPool pool $ do
                  void $ DB.runMigrationSilent migrateAll
                  readServerStore
              cacheVar <- newMVar store
              jwtKey <- Auth.generateKey
              let env =
                    ServerEnv
                      { serverEnvServerUUID = uuid
                      , serverEnvStoreCache = cacheVar
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
