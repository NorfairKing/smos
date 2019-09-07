module Smos.Sync.Server.Handler.PostSync
  ( handlePostSync
  ) where

import Smos.Sync.Server.Handler.Import

import Data.UUID.V4 as UUID

import Control.Concurrent.STM
import Control.Monad.Reader

import Data.Mergeful

handlePostSync :: SyncRequest UUID SyncFile -> SyncHandler (SyncResponse UUID SyncFile)
handlePostSync request = do
  var <- asks serverEnvStoreVar
  store <- liftIO $ readTVarIO var
  liftIO $ pPrint store
  liftIO $ pPrint request
  (resp, newStore) <- processServerSync (liftIO UUID.nextRandom) store request
  liftIO $ do
    atomically $ writeTVar var newStore
    saveStore newStore
  liftIO $ pPrint newStore
  liftIO $ pPrint resp
  pure resp
