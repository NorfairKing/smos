{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Sync.Client.Meta where

import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Mergeful as Mergeful
import Database.Persist.Sql as DB
import Path
import Smos.API.SHA256 as SHA256
import Smos.Client
import Smos.Sync.Client.Contents
import Smos.Sync.Client.DB
import Smos.Sync.Client.Env
import Smos.Sync.Client.MetaMap (MetaMap (..))
import qualified Smos.Sync.Client.MetaMap as MM
import Smos.Sync.Client.OptParse.Types

readClientMetadata :: MonadIO m => SqlPersistT m MetaMap
readClientMetadata = do
  cfs <- selectList [] []
  pure
    $ fromJust
    $ MM.fromList -- Safe because of DB constraints
    $ map
      ( \(Entity _ ClientFile {..}) ->
          ( clientFilePath,
            SyncFileMeta
              { syncFileMetaHash = clientFileSha256,
                syncFileMetaTime = clientFileTime
              }
          )
      )
      cfs

writeClientMetadata ::
  forall m.
  MonadIO m =>
  MetaMap ->
  SqlPersistT m ()
writeClientMetadata mm = do
  let m = metaMapFiles mm
  deleteWhere ([] :: [Filter ClientFile]) -- Clean slate
  void $ M.traverseWithKey go m
  where
    go :: Path Rel File -> SyncFileMeta -> SqlPersistT m ()
    go path SyncFileMeta {..} =
      void $
        upsertBy
          (UniquePath path)
          ( ClientFile
              { clientFilePath = path,
                clientFileSha256 = syncFileMetaHash,
                clientFileTime = syncFileMetaTime
              }
          )
          [ ClientFileSha256 =. syncFileMetaHash,
            ClientFileTime =. syncFileMetaTime
          ]

-- | We only check the synced items, because it should be the case that
-- they're the only ones that are not empty.
makeClientMetaData :: IgnoreFiles -> ClientStore -> Maybe MetaMap
makeClientMetaData igf ClientStore {..} =
  let Mergeful.ClientStore {..} = clientStoreItems
   in if not
        ( null clientStoreAddedItems
            && null clientStoreDeletedItems
            && null clientStoreSyncedButChangedItems
        )
        then Nothing
        else
          let go :: MetaMap -> Path Rel File -> Mergeful.Timed SyncFile -> Maybe MetaMap
              go m path Mergeful.Timed {..} =
                let SyncFile {..} = timedValue
                    goOn =
                      MM.insert
                        path
                        SyncFileMeta
                          { syncFileMetaTime = timedTime,
                            syncFileMetaHash = SHA256.hashBytes syncFileContents
                          }
                        m
                 in case igf of
                      IgnoreNothing -> goOn
                      IgnoreHiddenFiles ->
                        if isHidden path
                          then Just m
                          else goOn
           in foldM (\m (f, t) -> go m f t) MM.empty $ M.toList clientStoreSyncedItems
