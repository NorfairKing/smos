{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.Admin.GetUsers
  ( serveGetUsers,
  )
where

import Smos.Server.Handler.Import

serveGetUsers :: AdminCookie -> ServerHandler [UserInfo]
serveGetUsers AdminCookie {..} = do
  userEntities <- runDB $ selectList [] [Desc UserCreated]
  mServerAdmin <- asks serverEnvAdmin
  pure $
    flip map userEntities $ \(Entity _ User {..}) ->
      UserInfo
        { userInfoUsername = userName,
          userInfoAdmin = mServerAdmin == Just userName
        }
