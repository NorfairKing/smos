{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Backup where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Monoid (Sum (..))
import Data.Time
import Data.UUID.Typed (nextRandomUUID)
import Database.Persist
import Database.Persist.Sql
import Smos.API
import Smos.Server.DB

doBackupForUser :: MonadUnliftIO m => Int -> UserId -> SqlPersistT m BackupUUID
doBackupForUser compressionLevel uid = do
  now <- liftIO getCurrentTime
  acqFileSource <- selectSourceRes [ServerFileUser ==. uid] []

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

  Sum size <- withAcquire acqFileSource $ \source -> do
    let insertAndCount (Entity _ ServerFile {..}) = do
          let compressedContents = compressByteString compressionLevel serverFileContents
          insert_ BackupFile {backupFileBackup = backupId, backupFilePath = serverFilePath, backupFileContents = compressedContents}
          pure $ Sum $ compressedSize compressedContents

    runConduit $ source .| C.mapM insertAndCount .| C.fold

  update backupId [BackupSize =. size]

  pure uuid

deleteBackupById :: MonadIO m => BackupId -> SqlPersistT m ()
deleteBackupById bid = do
  deleteWhere [BackupFileBackup ==. bid]
  delete bid
