{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.FileMigration where

import Conduit
import Control.Monad.Logger
import qualified Data.Conduit.Combinators as C
import Data.Mergeful as Mergeful
import qualified Data.Text as T
import Database.Persist.Pagination
import Database.Persist.Sql
import Path
import Smos.Data
import Smos.Server.DB

runFileMigrationForUser :: (MonadUnliftIO m, MonadLogger m) => UserId -> SqlPersistT m ()
runFileMigrationForUser uid = do
  logInfoNS "file-migration" $ "Starting server file format migration for user: " <> T.pack (show (fromSqlKey uid))
  runConduit $
    streamEntities [ServerFileUser ==. uid] ServerFileId (PageSize 256) Ascend (Range Nothing Nothing)
      .| C.mapM_ refreshServerFile

  logInfoNS "file-migration" $ "Server file format migration done for user: " <> T.pack (show (fromSqlKey uid))

refreshServerFile :: (MonadIO m, MonadLogger m) => Entity ServerFile -> SqlPersistT m ()
refreshServerFile (Entity sfid ServerFile {..}) = do
  logInfoNS "file-migration" $ "Starting file migration for file: " <> T.pack (show serverFilePath)
  case fileExtension serverFilePath of
    Just ".smos" ->
      case parseSmosFile serverFileContents of
        Left err ->
          -- Not a parsable smos file, just leave it
          logWarnNS "file-migration" $
            T.unwords
              [ "Server file",
                T.pack (fromRelFile serverFilePath),
                "for user",
                T.pack (show (fromSqlKey serverFileUser)),
                "is not a valid smos file:",
                T.pack err
              ]
        Right sf -> do
          let newContents = smosFileBS sf -- Re-render file
          if newContents == serverFileContents
            then pure () -- wouldn't be an update, no need to update
            else do
              logInfoNS "file-migration" $
                T.unwords
                  [ "Migrating server file",
                    T.pack (fromRelFile serverFilePath),
                    "for user",
                    T.pack (show (fromSqlKey serverFileUser))
                  ]
              update
                sfid
                [ ServerFileContents =. newContents,
                  ServerFileTime =. Mergeful.incrementServerTime serverFileTime
                ]
    _ -> pure () -- Not a smos file, just leave it
