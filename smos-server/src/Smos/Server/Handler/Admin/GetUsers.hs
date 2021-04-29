{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.Admin.GetUsers
  ( serveGetUsers,
  )
where

import Smos.Server.Handler.Import

serveGetUsers :: AuthCookie -> ServerHandler [UserInfo]
serveGetUsers AuthCookie {..} = asAdmin authCookieUsername $ do
  userEntities <- runDB $ selectList [] [Desc UserCreated]
  mServerAdmin <- asks serverEnvAdmin
  pure $
    flip map userEntities $ \(Entity _ User {..}) ->
      UserInfo
        { userInfoUsername = userName,
          userInfoAdmin = mServerAdmin == Just userName,
          userInfoCreated = userCreated,
          userInfoLastLogin = userLastLogin
        }
