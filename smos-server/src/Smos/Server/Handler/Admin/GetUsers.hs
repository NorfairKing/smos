{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.Admin.GetUsers
  ( serveGetUsers,
  )
where

import Smos.Server.Handler.Import

serveGetUsers :: AuthNCookie -> ServerHandler [UserInfo]
serveGetUsers ac = asAdmin (authNCookieUsername ac) $ do
  userEntities <- runDB $ selectList [] [Desc UserCreated]
  mServerAdmin <- asks serverEnvAdmin
  pure $
    flip map userEntities $ \(Entity _ User {..}) ->
      UserInfo
        { userInfoUsername = userName,
          userInfoAdmin = mServerAdmin == Just userName,
          userInfoCreated = userCreated,
          userInfoLastLogin = userLastLogin,
          userInfoLastUse = userLastUse
        }
