{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Server.Env where

import GHC.Generics (Generic)

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB
import Data.UUID as X
import Data.UUID.V4 as UUID

import Text.Show.Pretty as X

import Path

import System.Exit

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.Reader

import Database.Persist as DB
import Database.Persist.Sql as DB

import qualified Data.Mergeful as Mergeful

import Servant

import Path.IO

import Smos.Sync.API as X

import Smos.Sync.Server.Data

type SyncHandler = ReaderT ServerEnv Handler

data ServerEnv =
  ServerEnv
    { serverEnvServerUUID :: UUID
    , serverEnvStoreCache :: MVar (Mergeful.ServerStore UUID SyncFile)
    , serverEnvConnection :: DB.ConnectionPool
    }
  deriving (Generic)

runDB :: DB.SqlPersistT IO a -> SyncHandler a
runDB func = do
  pool <- asks serverEnvConnection
  liftIO $ DB.runSqlPool func pool

readServerStore :: MonadIO m => SqlPersistT m (Mergeful.ServerStore UUID SyncFile)
readServerStore = undefined

saveStore :: MonadIO m => Mergeful.ServerStore UUID SyncFile -> SqlPersistT m ()
saveStore = undefined
