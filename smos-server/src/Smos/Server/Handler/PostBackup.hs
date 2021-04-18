{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PostBackup
  ( servePostBackup,
  )
where

import qualified Data.ByteString as SB
import Data.Foldable
import Smos.Server.Handler.Import

servePostBackup :: AuthCookie -> ServerHandler BackupUUID
servePostBackup (AuthCookie un) = withUserId un $ \uid -> do
  now <- liftIO getCurrentTime
  uuid <- nextRandomUUID
  maxBackups <- asks serverEnvMaxBackupsPerUser
  numberOfBackupsThatWeAlreadyHave <- runDB $ count [BackupUser ==. uid]
  -- The fromIntegral is safe because no one would put in a number between 2^63 and 2^64.
  when (maybe False ((numberOfBackupsThatWeAlreadyHave >=) . fromIntegral) maxBackups) $ throwError err403 {errBody = "Already made the maximum number of backups."}
  maxBackupSize <- asks serverEnvMaxBackupSizePerUser
  sumOfBackupsThatWeAlreadyHave <- runDB $ foldl' (+) 0 . map (backupSize . entityVal) <$> selectList [BackupUser ==. uid] []
  serverFiles <- runDB $ selectList [ServerFileUser ==. uid] []
  -- The fromIntegral is safe because it is Int -> Word64
  let size = foldl' (+) 0 $ map (fromIntegral . SB.length . serverFileContents . entityVal) serverFiles
  when (maybe False (sumOfBackupsThatWeAlreadyHave + size >=) maxBackupSize) $ throwError err403 {errBody = "No space for another backup."}
  runDB $ do
    backupId <-
      insert
        Backup
          { backupUser = uid,
            backupUuid = uuid,
            backupTime = now,
            backupSize = size
          }
    insertMany_ $
      flip map serverFiles $ \(Entity _ ServerFile {..}) ->
        BackupFile
          { backupFileBackup = backupId,
            backupFilePath = serverFilePath,
            backupFileContents = serverFileContents
          }
  pure uuid
