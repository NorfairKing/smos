{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.DeleteBackup
  ( serveDeleteBackup,
  )
where

import Smos.Server.Backup
import Smos.Server.Handler.Import

serveDeleteBackup :: AuthNCookie -> BackupUUID -> ServerHandler NoContent
serveDeleteBackup AuthNCookie {..} uuid = withUserId authCookieUsername $ \uid -> do
  mBackup <- runDB $ getBy $ UniqueBackupUUID uid uuid
  case mBackup of
    Nothing -> throwError err404
    Just (Entity bid _) -> do
      runDB $ deleteBackupById bid
      pure NoContent
