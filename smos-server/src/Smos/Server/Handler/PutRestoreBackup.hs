module Smos.Server.Handler.PutRestoreBackup
  ( servePutRestoreBackup,
  )
where

import Smos.Server.Handler.Import

servePutRestoreBackup :: AuthCookie -> BackupUUID -> ServerHandler NoContent
servePutRestoreBackup (AuthCookie un) = undefined
