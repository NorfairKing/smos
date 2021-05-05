module Smos.Server.Handler.DeleteUser
  ( serveDeleteUser,
  )
where

import Smos.Server.Backup
import Smos.Server.Handler.Import

serveDeleteUser :: AuthNCookie -> ServerHandler NoContent
serveDeleteUser ac = withUserId ac $ \uid -> do
  runDB $ do
    -- TODO do this in a conduit way?
    backupIds <- selectKeysList [BackupUser ==. uid] []
    mapM_ deleteBackupById backupIds
    deleteWhere [ServerFileUser ==. uid]
    delete uid
  pure NoContent
