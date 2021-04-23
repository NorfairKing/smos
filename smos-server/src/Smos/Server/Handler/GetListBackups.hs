{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetListBackups
  ( serveGetListBackups,
  )
where

import Smos.Server.Handler.Import

serveGetListBackups :: AuthCookie -> ServerHandler [BackupInfo]
serveGetListBackups AuthCookie {..} = withUserId authCookieUsername $ \uid ->
  runDB $ do
    backupEntities <- selectList [BackupUser ==. uid] [Asc BackupTime]
    pure $
      flip map backupEntities $ \(Entity _ Backup {..}) ->
        BackupInfo
          { backupInfoUUID = backupUuid,
            backupInfoTime = backupTime,
            backupInfoSize = backupSize
          }
