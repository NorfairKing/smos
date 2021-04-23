{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.DeleteBackup
  ( serveDeleteBackup,
  )
where

import Smos.Server.Backup
import Smos.Server.Handler.Import

serveDeleteBackup :: AuthCookie -> BackupUUID -> ServerHandler NoContent
serveDeleteBackup AuthCookie {..} uuid = withUserId authCookieUsername $ \uid -> do
  mBackup <- runDB $ getBy $ UniqueBackupUUID uid uuid
  case mBackup of
    Nothing -> throwError err404
    Just (Entity bid _) -> do
      runDB $ deleteBackupById bid
      pure NoContent
