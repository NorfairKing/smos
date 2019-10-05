{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Sync.Server.Env where

import GHC.Generics (Generic)

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import Data.UUID as X
import Data.UUID.V4 as UUID

import Path

import System.Exit

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.Reader

import Database.Persist as DB
import Database.Persist.Sql as DB

import qualified Data.Mergeful as Mergeful
import Data.Mergeful.Collection (ServerStore(..))
import Data.Mergeful.Timed (Timed(..))

import Servant

import Path.IO

import Smos.Sync.API

import Smos.Sync.Server.DB
import Smos.Sync.Server.Data

type SyncHandler = ReaderT ServerEnv Handler

data ServerEnv =
  ServerEnv
    { serverEnvServerUUID :: ServerUUID
    , serverEnvStoreCache :: MVar (Mergeful.ServerStore FileUUID SyncFile)
    , serverEnvConnection :: DB.ConnectionPool
    }
  deriving (Generic)

runDB :: DB.SqlPersistT IO a -> SyncHandler a
runDB func = do
  pool <- asks serverEnvConnection
  liftIO $ DB.runSqlPool func pool

readServerStore :: MonadIO m => SqlPersistT m (Mergeful.ServerStore FileUUID SyncFile)
readServerStore = do
  sfs <- selectList [] []
  pure $
    ServerStore $
    M.fromList $
    map
      (\(Entity _ ServerFile {..}) ->
         ( serverFileUuid
         , Timed
             { timedValue =
                 SyncFile {syncFilePath = serverFilePath, syncFileContents = serverFileContents}
             , timedTime = serverFileTime
             }))
      sfs

saveStore ::
     forall m. MonadIO m
  => Mergeful.ServerStore FileUUID SyncFile
  -> SqlPersistT m ()
saveStore = void . M.traverseWithKey go . serverStoreItems
  where
    go :: FileUUID -> Timed SyncFile -> SqlPersistT m ()
    go u Timed {..} =
      let SyncFile {..} = timedValue
       in void $
          upsertBy
            (UniquePath syncFilePath)
            (ServerFile
               { serverFileUuid = u
               , serverFilePath = syncFilePath
               , serverFileContents = syncFileContents
               , serverFileTime = timedTime
               })
            [ServerFileContents =. syncFileContents, ServerFileTime =. timedTime]
