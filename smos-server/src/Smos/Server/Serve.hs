{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Serve where

import Crypto.JOSE.JWK (JWK)
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON (encodePretty)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Path
import Path.IO
import System.Exit

import Control.Concurrent.MVar
import Control.Monad.Logger
import Control.Monad.Reader

import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp

import Database.Persist.Sqlite as DB

import Servant.API.Generic
import Servant.Auth.Server as Auth
import Servant.Server as Servant
import Servant.Server.Generic

import Smos.API
import Smos.Server.Handler
import Smos.Server.OptParse

serveSmosSyncServer :: ServeSettings -> IO ()
serveSmosSyncServer ss@ServeSettings {..} = do
  pPrint ss
  runStderrLoggingT $
    DB.withSqlitePool (T.pack $ fromAbsFile serveSetDatabaseFile) 1 $ \pool ->
      liftIO $ do
        uuid <- readServerUUID serveSetUUIDFile
        store <-
          flip DB.runSqlPool pool $ do
            DB.runMigration migrateAll
            readServerStore
        cacheVar <- newMVar store
        jwtKey <- loadSigningKey
        let env =
              ServerEnv
                { serverEnvServerUUID = uuid
                , serverEnvStoreCache = cacheVar
                , serverEnvConnection = pool
                , serverEnvCookieSettings = defaultCookieSettings
                , serverEnvJWTSettings = defaultJWTSettings jwtKey
                }
        Warp.run serveSetPort $ makeSyncApp env

-- TODO put this file in settings
signingKeyFile :: IO (Path Abs File)
signingKeyFile = resolveFile' "signing-key.json"

storeSigningKey :: JWK -> IO ()
storeSigningKey key_ = do
  skf <- signingKeyFile
  LB.writeFile (toFilePath skf) (JSON.encodePretty key_)

loadSigningKey :: IO JWK
loadSigningKey = do
  skf <- signingKeyFile
  mErrOrKey <- forgivingAbsence $ JSON.eitherDecode <$> LB.readFile (toFilePath skf)
  case mErrOrKey of
    Nothing -> do
      key_ <- Auth.generateKey
      storeSigningKey key_
      pure key_
    Just (Left err) ->
      die $ unlines ["Failed to load signing key from file", fromAbsFile skf, "with error:", err]
    Just (Right r) -> pure r

makeSyncApp :: ServerEnv -> Wai.Application
makeSyncApp env =
  let cfg = serverEnvCookieSettings env :. serverEnvJWTSettings env :. EmptyContext
   in Servant.serveWithcontext syncAPI cfg $
      hoistServerWithContext
        syncAPI
        (Proxy :: Proxy '[ CookieSettings, JWTSettings])
        ((`runReaderT` env) :: SyncHandler a -> Handler a)
        syncServantServer

syncServantServer :: ServerT SyncAPI SyncHandler
syncServantServer = toServant syncServerRecord

syncServerRecord :: APIRoutes (AsServerT SyncHandler)
syncServerRecord =
  APIRoutes
    { unprotectedRoutes = toServant syncServerUnprotectedRoutes
    , protectedRoutes = toServant syncServerProtectedRoutes
    }

syncServerUnprotectedRoutes :: UnprotectedRoutes (AsServerT SyncHandler)
syncServerUnprotectedRoutes =
  UnprotectedRoutes {postRegister = servePostRegister, postLogin = servePostLogin}

syncServerProtectedRoutes :: ProtectedRoutes (AsServerT SyncHandler)
syncServerProtectedRoutes = ProtectedRoutes {postSync = servePostSync}

readServerUUID :: Path Abs File -> IO ServerUUID
readServerUUID p = do
  mContents <- forgivingAbsence $ LB.readFile $ fromAbsFile p
  case mContents of
    Nothing -> do
      u <- nextRandomUUID
      writeServerUUID p u
      pure u
    Just contents ->
      case JSON.eitherDecode contents of
        Left err -> die err
        Right u -> pure u

writeServerUUID :: Path Abs File -> ServerUUID -> IO ()
writeServerUUID p u = do
  ensureDir (parent p)
  LB.writeFile (fromAbsFile p) $ JSON.encodePretty u
