module Smos.Server.Handler.Admin.GetUsers
  ( serveGetUsers,
  )
where

import Smos.Server.Handler.Import

serveGetUsers :: AdminCookie -> ServerHandler [UserInfo]
serveGetUsers _ = pure []
