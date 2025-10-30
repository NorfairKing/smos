{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PutRestoreBackup
  ( servePutRestoreBackup,
  )
where

import qualified Data.Map as M
import Data.Mergeful.Timed
import Database.Persist as DB
import Smos.Server.Handler.Import
import Smos.Server.Subscription

servePutRestoreBackup :: AuthNCookie -> BackupUUID -> ServerHandler NoContent
servePutRestoreBackup ac uuid = withUserId ac $ \uid -> withSubscription ac $ do
  mBackup <- runDB $ getBy $ UniqueBackupUUID uid uuid
  case mBackup of
    Nothing -> throwError err404
    Just (Entity bid _) -> do
      runDB $ do
        -- Get the current files, so we can increment the server times
        currentServerFiles <- selectList [ServerFileUser ==. uid] []
        let currentServerFilesMap = M.fromList $ map (\(Entity _ ServerFile {..}) -> (serverFilePath, serverFileTime)) currentServerFiles
        -- Delete the current files, so no files that don't exist in the backup
        -- are leftover afterwards
        deleteWhere [ServerFileUser ==. uid]
        -- Make a new server file for each backup file
        backupFiles <- selectList [BackupFileBackup ==. bid] []
        forM_ backupFiles $ \(Entity _ BackupFile {..}) -> do
          mCasFile <- DB.get backupFileFile
          forM_ mCasFile $ \CasFile {..} -> do
            DB.insert_
              ServerFile
                { serverFileUser = uid,
                  serverFilePath = backupFilePath,
                  serverFileContents = decompressByteStringOrErrorMessage casFileContents,
                  serverFileTime = case M.lookup backupFilePath currentServerFilesMap of
                    Nothing -> initialServerTime
                    Just time -> incrementServerTime time
                }
      pure NoContent
