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
import Smos.Server.Subscription

servePostBackup :: AuthNCookie -> ServerHandler BackupUUID
servePostBackup ac = withUserId ac $ \uid -> withSubscription ac $ do
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
