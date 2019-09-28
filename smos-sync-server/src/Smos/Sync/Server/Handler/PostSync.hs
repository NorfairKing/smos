{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Server.Handler.PostSync
  ( handlePostSync
  ) where

import Smos.Sync.Server.Handler.Import

import Data.UUID.V4 as UUID

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.Reader

import qualified Data.Mergeful as Mergeful

handlePostSync :: SyncRequest -> SyncHandler SyncResponse
handlePostSync request = do
  ServerEnv {..} <- ask
  liftIO $
    withMVar serverEnvStoreLock $ \() -> do
      store <- liftIO $ readTVarIO serverEnvStoreVar
      (items, newStore) <- Mergeful.processServerSync (liftIO UUID.nextRandom) store request
      liftIO $ do
        atomically $ writeTVar serverEnvStoreVar newStore
        let newServerStore =
              ServerStore {serverStoreServerUUID = serverEnvServerUUID, serverStoreItems = newStore}
        saveStore serverEnvStoreFile newServerStore
      let resp =
            SyncResponse {syncResponseServerId = serverEnvServerUUID, syncResponseItems = items}
      pure resp
