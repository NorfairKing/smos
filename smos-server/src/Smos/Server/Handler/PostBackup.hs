{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PostBackup
  ( servePostBackup,
  )
where

import qualified Data.ByteString as SB
import Smos.Server.Handler.Import

servePostBackup :: AuthCookie -> ServerHandler BackupUUID
servePostBackup (AuthCookie un) = withUserId un $ \uid -> do
  now <- liftIO getCurrentTime
  uuid <- nextRandomUUID
  runDB $ do
    serverFiles <- selectList [ServerFileUser ==. uid] []
    let size = foldl' (+) 0 $ map (SB.length . serverFileContents . entityVal) serverFiles
    backupId <-
      insert
        Backup
          { backupUser = uid,
            backupUuid = uuid,
            backupTime = now,
            backupSize = fromIntegral size -- Safe because it is Int -> Word64
          }
    insertMany_ $
      flip map serverFiles $ \(Entity _ ServerFile {..}) ->
        BackupFile
          { backupFileBackup = backupId,
            backupFilePath = serverFilePath,
            backupFileContents = serverFileContents
          }
  pure uuid
