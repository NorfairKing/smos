{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetUserPermissions
  ( serveGetUserPermissions,
  )
where

import Smos.Server.Handler.Import

serveGetUserPermissions :: AuthNCookie -> ServerHandler UserPermissions
serveGetUserPermissions AuthNCookie {..} = do
  mAdmin <- asks serverEnvAdmin
  pure
    UserPermissions
      { userPermissionsIsAdmin = mAdmin == Just authCookieUsername
      }
