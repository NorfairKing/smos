module Smos.Server.Handler.PostBackup
  ( servePostBackup,
  )
where

import Smos.Server.Handler.Import

servePostBackup :: AuthCookie -> ServerHandler BackupUUID
servePostBackup (AuthCookie un) = undefined
