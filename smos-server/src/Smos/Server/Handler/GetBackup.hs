{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetBackup
  ( serveGetBackup,
  )
where

import Codec.Archive.Zip as Zip
import Data.ByteString (ByteString)
import Servant.Types.SourceT as Source
import Smos.Server.Handler.Import
import UnliftIO

serveGetBackup :: AuthCookie -> BackupUUID -> ServerHandler (SourceIO ByteString)
serveGetBackup (AuthCookie un) uuid = withUserId un $ \uid -> do
  mBackup <- runDB $ getBy $ UniqueBackupUUID uid uuid
  case mBackup of
    Nothing -> throwError err404
    Just (Entity bid _) -> do
      systemTempDir <- getTempDir
      tmpDir <- createTempDir systemTempDir "smos-server-temp-archive-dir"
      tempArchiveFileName <- resolveFile tmpDir "backup.zip"
      backupFiles <- runDB $ selectList [BackupFileBackup ==. bid] []
      -- Create a zip archive
      Zip.createArchive (fromAbsFile tempArchiveFileName) $
        forM_ backupFiles $ \(Entity _ BackupFile {..}) -> do
          selector <- Zip.mkEntrySelector (fromRelFile backupFilePath)
          let contents = backupFileContents
          Zip.addEntry BZip2 contents selector
      -- Stream the zip archive
      pure $
        SourceT $ \step -> do
          let SourceT func = Source.readFile (fromAbsFile tempArchiveFileName)
          func step `finally` removeDirRecur tmpDir
