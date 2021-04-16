module Smos.Server.Handler.GetBackup
  ( serveGetBackup,
  )
where

import Data.ByteString (ByteString)
import Smos.Server.Handler.Import

serveGetBackup :: AuthCookie -> BackupUUID -> ServerHandler (SourceIO ByteString)
serveGetBackup (AuthCookie un) p = undefined
