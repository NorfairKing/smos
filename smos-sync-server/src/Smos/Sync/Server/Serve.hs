{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Server.Serve where

import Control.Concurrent.STM
import Control.Monad.Reader

import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp

import Servant

import Smos.Sync.API
import Smos.Sync.Server.Handler
import Smos.Sync.Server.OptParse

serveSmosSyncServer :: ServeSettings -> IO ()
serveSmosSyncServer ss@ServeSettings {..} = do
  pPrint ss
  store@ServerStore {..} <- readStore serveSetStoreFile
  var <- newTVarIO serverStoreItems
  let env =
        ServerEnv
          { serverEnvServerUUID = serverStoreServerUUID
          , serverEnvStoreFile = serveSetStoreFile
          , serverEnvStoreVar = var
          }
  saveStore  serveSetStoreFile store
  Warp.run serveSetPort $ makeSyncApp env

makeSyncApp :: ServerEnv -> Wai.Application
makeSyncApp env =
  Servant.serve syncAPI $
  hoistServer syncAPI ((\func -> runReaderT func env) :: SyncHandler a -> Handler a) syncServer

syncServer :: ServerT SyncAPI SyncHandler
syncServer = handlePostSync
