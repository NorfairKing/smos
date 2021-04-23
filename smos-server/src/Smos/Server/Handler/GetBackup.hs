{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetBackup
  ( serveGetBackup,
    prepareBackupFilePath,
  )
where

import Codec.Archive.Zip as Zip
import Data.ByteString (ByteString)
import Servant.Types.SourceT as Source
import Smos.Server.Handler.Import
import System.FilePath.Posix as Posix
import System.FilePath.Windows as Windows
import UnliftIO

serveGetBackup :: AuthCookie -> BackupUUID -> ServerHandler (SourceIO ByteString)
serveGetBackup AuthCookie {..} uuid = withUserId authCookieUsername $ \uid -> do
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
          -- NOTE: Running 'Zip.mkEntrySelector' in 'Maybe' instead of 'IO'
          -- (because it can be run in any 'MonadThrow') means that we could be
          -- missing files in the resulting zip file.  However: We think that
          -- that is better than not being able to download any backups.
          let mSelector :: Maybe Zip.EntrySelector
              mSelector = Zip.mkEntrySelector (prepareBackupFilePath backupFilePath)
          let contents = decompressByteStringOrErrorMessage backupFileContents
          forM mSelector $ Zip.addEntry Zip.Deflate contents
      -- Stream the zip archive
      pure $
        SourceT $ \step -> do
          let SourceT func = Source.readFile (fromAbsFile tempArchiveFileName)
          func step `finally` removeDirRecur tmpDir

prepareBackupFilePath :: Path Rel File -> FilePath
prepareBackupFilePath = Windows.makeValid . Posix.makeValid . fromRelFile
