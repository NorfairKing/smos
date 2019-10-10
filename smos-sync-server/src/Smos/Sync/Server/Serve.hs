{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Server.Serve where

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON (encodePretty)
import qualified Data.ByteString.Lazy as LB
import Data.Text as T
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
import Servant.Server as Servant
import Servant.Server.Generic

import Smos.Sync.API
import Smos.Sync.Server.Handler
import Smos.Sync.Server.OptParse

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
        let env =
              ServerEnv
                { serverEnvServerUUID = uuid
                , serverEnvStoreCache = cacheVar
                , serverEnvConnection = pool
                }
        Warp.run serveSetPort $ makeSyncApp env

makeSyncApp :: ServerEnv -> Wai.Application
makeSyncApp env =
  Servant.serve syncAPI $
  hoistServer syncAPI ((`runReaderT` env) :: SyncHandler a -> Handler a) syncServantServer

syncServantServer :: ServerT SyncAPI SyncHandler
syncServantServer = toServant syncServerRecord

syncServerRecord :: APIRoutes (AsServerT SyncHandler)
syncServerRecord =
  APIRoutes
    { unprotectedRoutes = toServant syncServerUnprotectedRoutes
    , protectedRoutes = toServant syncServerProtectedRoutes
    }

syncServerUnprotectedRoutes :: UnprotectedRoutes (AsServerT SyncHandler)
syncServerUnprotectedRoutes = UnprotectedRoutes {postRegister = handlePostRegister}

syncServerProtectedRoutes :: ProtectedRoutes (AsServerT SyncHandler)
syncServerProtectedRoutes = ProtectedRoutes {postSync = handlePostSync}

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
