module Smos.Server.Handler.DeleteBackup
  ( serveDeleteBackup,
  )
where

import Smos.Server.Handler.Import

serveDeleteBackup :: AuthCookie -> BackupUUID -> ServerHandler NoContent
serveDeleteBackup (AuthCookie un) uuid = withUserId un $ \uid -> do
  mBackup <- runDB $ getBy $ UniqueBackupUUID uid uuid
  case mBackup of
    Nothing -> throwError err404
    Just (Entity bid _) -> do
      runDB $ do
        deleteWhere [BackupFileBackup ==. bid]
        delete bid
      pure NoContent
