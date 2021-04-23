{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PutRestoreBackup
  ( servePutRestoreBackup,
  )
where

import qualified Data.Map as M
import Data.Mergeful.Timed
import Smos.Server.Handler.Import

servePutRestoreBackup :: AuthCookie -> BackupUUID -> ServerHandler NoContent
servePutRestoreBackup AuthCookie {..} uuid = withUserId authCookieUsername $ \uid -> do
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
        insertMany_ $
          flip map backupFiles $ \(Entity _ BackupFile {..}) ->
            ServerFile
              { serverFileUser = uid,
                serverFilePath = backupFilePath,
                serverFileContents = decompressByteStringOrErrorMessage backupFileContents,
                serverFileTime = case M.lookup backupFilePath currentServerFilesMap of
                  Nothing -> initialServerTime
                  Just time -> incrementServerTime time
              }
      pure NoContent
