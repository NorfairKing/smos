{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Backup where

import Control.Monad.Reader
import Data.Foldable
import qualified Data.Map as M
import Data.Time
import Data.UUID.Typed (nextRandomUUID)
import Database.Persist
import Database.Persist.Sql
import Smos.API
import Smos.Server.DB

doBackupForUser :: Int -> UserId -> SqlPersistT IO BackupUUID
doBackupForUser compressionLevel uid = do
  now <- liftIO getCurrentTime
  serverFiles <- selectList [ServerFileUser ==. uid] []

  let compressAndCount (!s, !cfs) (Entity _ ServerFile {..}) =
        let compressedFile = compressByteString compressionLevel serverFileContents
         in (s + compressedSize compressedFile, M.insert serverFilePath compressedFile cfs)

  let (size, compressedFiles) = foldl' compressAndCount (0, M.empty) serverFiles

  uuid <- nextRandomUUID

  -- Do the actual backup.
  backupId <-
    insert
      Backup
        { backupUser = uid,
          backupUuid = uuid,
          backupTime = now,
          backupSize = size
        }
  insertMany_ $
    flip map (M.toList compressedFiles) $ \(path, contents) ->
      BackupFile
        { backupFileBackup = backupId,
          backupFilePath = path,
          backupFileContents = contents
        }

  pure uuid

deleteBackupById :: BackupId -> SqlPersistT IO ()
deleteBackupById bid = do
  deleteWhere [BackupFileBackup ==. bid]
  delete bid
