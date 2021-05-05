{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.DeleteUser
  ( serveDeleteUser,
  )
where

import Smos.Server.Backup
import Smos.Server.Handler.Import

serveDeleteUser :: AuthNCookie -> ServerHandler NoContent
serveDeleteUser AuthNCookie {..} = withUserId authCookieUsername $ \uid -> do
  runDB $ do
    -- TODO do this in a conduit way?
    backupIds <- selectKeysList [BackupUser ==. uid] []
    mapM_ deleteBackupById backupIds
    deleteWhere [ServerFileUser ==. uid]
    delete uid
  pure NoContent
