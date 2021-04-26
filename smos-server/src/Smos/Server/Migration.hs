{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Migration (serverAutoMigration, serverStartupMigration) where

import Conduit
import Control.Monad.Logger
import qualified Data.Conduit.Combinators as C
import Data.Mergeful as Mergeful
import Database.Persist.Sql
import Path
import Smos.Data
import Smos.Server.DB

serverStartupMigration :: Int -> SqlPersistT (LoggingT IO) ()
serverStartupMigration compressionLevel = do
  logInfoNS "startup-migration" "Starting server file format migration"
  acqSource <- selectSourceRes [] [Asc ServerFileId]
  withAcquire acqSource $ \source ->
    runConduit $ source .| C.mapM_ refreshServerFile
  logInfoNS "startup-migration" "Server file format migration done"
  logInfoNS "startup-migration" "Starting backup file format migration"
  acqSource <- selectSourceRes [] [Asc BackupFileId]
  withAcquire acqSource $ \source ->
    runConduit $ source .| C.mapM_ (refreshBackupFile compressionLevel)
  logInfoNS "startup-migration" "Server backup file format migration done"

refreshServerFile :: Entity ServerFile -> SqlPersistT (LoggingT IO) ()
refreshServerFile (Entity sfid ServerFile {..}) =
  case fileExtension serverFilePath of
    Just ".smos" ->
      case parseSmosFile serverFileContents of
        Left _ -> pure () -- Not a parsable smos file, just leave it
        Right sf -> do
          let newContents = smosFileBS sf -- Re-render file
          if newContents == serverFileContents
            then pure () -- wouldn't be an update, no need to update
            else
              update
                sfid
                [ ServerFileContents =. newContents,
                  ServerFileTime =. Mergeful.incrementServerTime serverFileTime
                ]
    _ -> pure () -- Not a smos file, just leave it

refreshBackupFile :: Int -> Entity BackupFile -> SqlPersistT (LoggingT IO) ()
refreshBackupFile compressionLevel (Entity sfid BackupFile {..}) =
  case fileExtension backupFilePath of
    Just ".smos" ->
      case decompressByteString backupFileContents of
        Left _ -> pure () -- Not a valid compression of anything
        Right contents ->
          case parseSmosFile contents of
            Left _ -> pure () -- Not a parsable smos file, just leave it
            Right sf -> do
              let newContents = compressByteString compressionLevel (smosFileBS sf) -- Re-render file
              if newContents == backupFileContents
                then pure () -- wouldn't be an update, no need to update
                else
                  update
                    sfid
                    [ BackupFileContents =. newContents
                    ]
    _ -> pure () -- Not a smos file, just leave it
