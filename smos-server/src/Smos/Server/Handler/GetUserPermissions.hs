{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetUserPermissions
  ( serveGetUserPermissions,
  )
where

import Smos.Server.Handler.Import

serveGetUserPermissions :: AuthCookie -> ServerHandler UserPermissions
serveGetUserPermissions AuthCookie {..} = do
  mAdmin <- asks serverEnvAdmin
  pure
    UserPermissions
      { userPermissionsIsAdmin = mAdmin == Just authCookieUsername
      }
