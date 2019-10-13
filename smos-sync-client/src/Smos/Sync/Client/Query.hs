{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Sync.Client.Query where

import Data.Map (Map)
import qualified Data.Map as M

import Path

import Control.Monad.Reader

import Database.Persist.Sql as DB

import Smos.Sync.Client.DB
import Smos.Sync.Client.Env

readClientMetadata :: MonadIO m => SqlPersistT m (Map (Path Rel File) SyncFileMeta)
readClientMetadata = do
  cfs <- selectList [] []
  pure $
    M.fromList $
    map
      (\(Entity _ ClientFile {..}) ->
         ( clientFilePath
         , SyncFileMeta
             { syncFileMetaUUID = clientFileUuid
             , syncFileMetaHash = clientFileHash
             , syncFileMetaTime = clientFileTime
             }))
      cfs

writeClientMetadata ::
     forall m. MonadIO m
  => Map (Path Rel File) SyncFileMeta
  -> SqlPersistT m ()
writeClientMetadata m = do
  deleteWhere [ClientFilePath /<-. M.keys m]
  void $ M.traverseWithKey go m
  where
    go :: Path Rel File -> SyncFileMeta -> SqlPersistT m ()
    go path SyncFileMeta {..} =
      void $
      upsertBy
        (UniquePath path)
        (ClientFile
           { clientFileUuid = syncFileMetaUUID
           , clientFilePath = path
           , clientFileHash = syncFileMetaHash
           , clientFileTime = syncFileMetaTime
           })
        [ClientFileHash =. syncFileMetaHash, ClientFileTime =. syncFileMetaTime]
