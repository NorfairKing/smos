{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Migration (serverAutoMigration, serverStartupMigration) where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Mergeful as Mergeful
import Database.Persist.Sql
import Path
import Smos.Data
import Smos.Server.DB

serverStartupMigration :: SqlPersistT IO ()
serverStartupMigration = do
  acqSource <- selectSourceRes [] [Asc ServerFileId]
  withAcquire acqSource $ \source ->
    runConduit $ source .| C.mapM_ refreshServerFile

refreshServerFile :: Entity ServerFile -> SqlPersistT IO ()
refreshServerFile (Entity sfid ServerFile {..}) =
  case fileExtension serverFilePath of
    Just ".smos" ->
      case parseSmosFile serverFileContents of
        Left _ -> pure () -- Not a parsable smos file, just leave it
        Right sf -> do
          let newContents = smosFileBS sf
          if newContents == serverFileContents
            then pure () -- wouldn't be an update, no need to update
            else
              update
                sfid
                [ ServerFileContents =. newContents,
                  ServerFileTime =. Mergeful.incrementServerTime serverFileTime
                ]
    _ -> pure () -- Not a smos file, just leave it
