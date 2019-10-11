{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PostSync
  ( servePostSync
  ) where

import Smos.Server.Handler.Import

import Control.Concurrent.MVar

import Database.Persist.Sql as DB

import qualified Data.Mergeful as Mergeful

servePostSync :: AuthCookie -> SyncRequest -> SyncHandler SyncResponse
servePostSync (AuthCookie un) request = do
  ServerEnv {..} <- ask
  respItems <-
    liftIO $
    modifyMVar serverEnvStoreCache $ \store -> do
      (resp, newStore) <- Mergeful.processServerSync nextRandomUUID store request
      DB.runSqlPool (saveStore newStore) serverEnvConnection
      pure (newStore, resp)
  let resp =
        SyncResponse {syncResponseServerId = serverEnvServerUUID, syncResponseItems = respItems}
  pure resp
