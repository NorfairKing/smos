{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Server.Handler.PostSync
  ( handlePostSync
  ) where

import Smos.Sync.Server.Handler.Import

import Data.UUID.V4 as UUID

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.Reader

import Database.Persist.Sql as DB

import qualified Data.Mergeful as Mergeful

handlePostSync :: SyncRequest -> SyncHandler SyncResponse
handlePostSync request = do
  ServerEnv {..} <- ask
  respItems <-
    liftIO $
    modifyMVar serverEnvStoreCache $ \store -> do
      (resp, newStore) <- Mergeful.processServerSync (liftIO UUID.nextRandom) store request
      DB.runSqlPool (saveStore newStore) serverEnvConnection
      pure (newStore, resp)
  let resp =
        SyncResponse {syncResponseServerId = serverEnvServerUUID, syncResponseItems = respItems}
  pure resp
