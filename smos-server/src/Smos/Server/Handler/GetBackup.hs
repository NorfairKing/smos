{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetBackup
  ( serveGetBackup,
    prepareBackupFilePath,
  )
where

import Codec.Archive.Zip as Zip
import Conduit
import Data.ByteString (ByteString)
import qualified Data.Conduit.Combinators as C
import Database.Persist.Pagination
import qualified Database.Persist.Sql as DB
import qualified Database.Persist.Sqlite as DB
import Servant.Types.SourceT as Source
import Smos.Server.Handler.Import
import Smos.Server.Subscription
import System.FilePath.Posix as Posix
import System.FilePath.Windows as Windows
import UnliftIO

serveGetBackup :: AuthNCookie -> BackupUUID -> ServerHandler (SourceIO ByteString)
serveGetBackup ac uuid = withUserId ac $ \uid -> withSubscription ac $ do
  mBackup <- runDB $ getBy $ UniqueBackupUUID uid uuid
  case mBackup of
    Nothing -> throwError err404
    Just (Entity bid _) -> do
      systemTempDir <- getTempDir
      tmpDir <- createTempDir systemTempDir "smos-server-temp-archive-dir"
      tempArchiveFileName <- resolveFile tmpDir "backup.zip"
      pool <- asks serverEnvConnection
      logFunc <- asks serverEnvLogFunc
      -- Create a zip archive
      Zip.createArchive (fromAbsFile tempArchiveFileName) $ do
        let insertBackupFile (Entity _ BackupFile {..}) = do
              -- NOTE: Running 'Zip.mkEntrySelector' in 'Maybe' instead of 'IO'
              -- (because it can be run in any 'MonadThrow') means that we could be
              -- missing files in the resulting zip file.  However: We think that
              -- that is better than not being able to download any backups.
              let mSelector :: Maybe Zip.EntrySelector
                  mSelector = Zip.mkEntrySelector (prepareBackupFilePath backupFilePath)
              let contents = decompressByteStringOrErrorMessage backupFileContents
              forM_ mSelector $ Zip.addEntry Zip.Deflate contents
        runConduit $
          runDBArchiverConduit
            logFunc
            pool
            (streamEntities [BackupFileBackup ==. bid] BackupFileId (PageSize 256) Ascend (Range Nothing Nothing))
            .| C.mapM_ insertBackupFile
      -- Stream the zip archive
      pure $ streamArchive tmpDir tempArchiveFileName

{-# ANN runDBArchiverConduit ("NOCOVER" :: String) #-}
runDBArchiverConduit ::
  (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> DB.ConnectionPool -> ConduitT i o (DB.SqlPersistT (LoggingT IO)) r -> ConduitT i o ZipArchive r
runDBArchiverConduit logFunc pool =
  let runDBInArchiver :: DB.SqlPersistT (LoggingT IO) a -> ZipArchive a
      runDBInArchiver func = liftIO $ runLoggingT (DB.runSqlPool (DB.retryOnBusy func) pool) logFunc
   in transPipe runDBInArchiver

{-# ANN streamArchive ("NOCOVER" :: String) #-}
streamArchive :: Path Abs Dir -> Path Abs File -> SourceT IO ByteString
streamArchive tmpDir tempArchiveFileName =
  SourceT $ \step -> do
    let SourceT func = Source.readFile (fromAbsFile tempArchiveFileName)
    func step `finally` removeDirRecur tmpDir

prepareBackupFilePath :: Path Rel File -> FilePath
prepareBackupFilePath = Windows.makeValid . Posix.makeValid . fromRelFile
