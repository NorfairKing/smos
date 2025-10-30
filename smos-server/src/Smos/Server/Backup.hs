{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Server.Backup where

import Conduit
import Data.ByteString (ByteString)
import qualified Data.Conduit.Combinators as C
import Data.Monoid (Sum (..))
import Data.Time
import Data.UUID.Typed (nextRandomUUID)
import Data.Word
import qualified Database.Esqueleto.Experimental as E
import Database.Persist as DB
import Database.Persist.Pagination
import Database.Persist.Sql
import Smos.API
import Smos.Server.DB

doBackupForUser :: (MonadUnliftIO m) => Int -> UserId -> SqlPersistT m BackupUUID
doBackupForUser compressionLevel uid = do
  now <- liftIO getCurrentTime

  uuid <- nextRandomUUID

  -- Make a backup with an empty size, we'll fill it in when we've counted all the files.
  backupId <-
    insert
      Backup
        { backupUser = uid,
          backupUuid = uuid,
          backupTime = now,
          backupSize = 0 -- Temporarily
        }

  let insertAndCount (Entity _ ServerFile {..}) = do
        (casFileId, size) <- upsertCASFile compressionLevel serverFileContents
        _ <-
          DB.upsertBy
            (UniqueBackupFilePath backupId serverFilePath)
            BackupFile
              { backupFileBackup = backupId,
                backupFilePath = serverFilePath,
                backupFileFile = casFileId
              }
            []
        pure size

  Sum size <-
    runConduit $
      streamEntities [ServerFileUser ==. uid] ServerFileId (PageSize 256) Ascend (Range Nothing Nothing)
        .| C.mapM insertAndCount
        .| C.fold

  update backupId [BackupSize =. size]

  pure uuid

upsertCASFile :: (MonadIO m) => Int -> ByteString -> SqlPersistT m (CasFileId, Sum Word64)
upsertCASFile compressionLevel contents = do
  let hash = hashContents contents
  mExisting <- E.selectOne $ do
    casFile <- E.from $ E.table @CasFile
    E.where_ (casFile E.^. CasFileHash E.==. E.val hash)
    pure (casFile E.^. CasFileId, E.length_ (casFile E.^. CasFileContents))
  case mExisting of
    Just (E.Value casFileId, E.Value size) ->
      pure
        ( casFileId,
          Sum size
        )
    Nothing -> do
      let compressedContents = compressByteString compressionLevel contents
      let casFile =
            CasFile
              { casFileHash = hash,
                casFileContents = compressedContents
              }
      casFileId <- DB.insert casFile
      pure (casFileId, Sum $ compressedSize compressedContents)
