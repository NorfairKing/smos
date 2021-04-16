module Smos.Server.Handler.GetListBackups
  ( serveGetListBackups,
  )
where

import Smos.Server.Handler.Import

serveGetListBackups :: AuthCookie -> ServerHandler [Backup]
serveGetListBackups (AuthCookie _) = undefined
