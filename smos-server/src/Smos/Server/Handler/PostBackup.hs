{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PostBackup
  ( servePostBackup,
  )
where

import Data.Foldable
import qualified Data.Map as M
import Smos.Server.Backup
import Smos.Server.Handler.Import

servePostBackup :: AuthCookie -> ServerHandler BackupUUID
servePostBackup (AuthCookie un) = withUserId un $ \uid -> do
  maxBackups <- asks serverEnvMaxBackupsPerUser
  numberOfBackupsThatWeAlreadyHave <- runDB $ count [BackupUser ==. uid]
  -- Don't let the backup happen if we already have the maximum number of backups for this user.
  -- The fromIntegral is safe because no one would put in a number between 2^63 and 2^64.
  when (maybe False ((numberOfBackupsThatWeAlreadyHave >=) . fromIntegral) maxBackups) $ throwError err403 {errBody = "Already made the maximum number of backups."}
  maxBackupSize <- asks serverEnvMaxBackupSizePerUser
  sumOfBackupsThatWeAlreadyHave <- runDB $ foldl' (+) 0 . map (backupSize . entityVal) <$> selectList [BackupUser ==. uid] []

  -- Figure out how large the backup will be and get the files compressed so that we can save them that way
  serverFiles <- runDB $ selectList [ServerFileUser ==. uid] []
  compressionLevel <- asks serverEnvCompressionLevel
  let compressAndCount (!s, !cfs) (Entity _ ServerFile {..}) =
        let compressedFile = compressByteString compressionLevel serverFileContents
         in (s + compressedSize compressedFile, M.insert serverFilePath compressedFile cfs)
  let (size, _) = foldl' compressAndCount (0, M.empty) serverFiles
  -- Don't let a backup happen if we would be storing more than the server allows
  when (maybe False (sumOfBackupsThatWeAlreadyHave + size >=) maxBackupSize) $ throwError err403 {errBody = "No space for another backup."}

  -- Do the actual backup.
  runDB $ doBackupForUser compressionLevel uid
