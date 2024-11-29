module Smos.Server.Handler.DeleteUser
  ( serveDeleteUser,
  )
where

import Smos.Server.Handler.Import

serveDeleteUser :: AuthNCookie -> ServerHandler NoContent
serveDeleteUser ac = withUserId ac $ \uid -> do
  runDB $ delete uid
  pure NoContent
