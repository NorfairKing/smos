{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.Admin.GetUser
  ( serveGetUser,
  )
where

import Smos.Server.Handler.Import
import Smos.Server.Subscription

serveGetUser :: AuthNCookie -> Username -> ServerHandler UserInfo
serveGetUser ac username = asAdmin (authNCookieUsername ac) $ do
  mUser <- runDB $ getBy $ UniqueUsername username
  case mUser of
    Nothing -> throwError $ err404 {errBody = "User not found."}
    Just (Entity uid User {..}) -> do
      mServerAdmin <- asks serverEnvAdmin
      subscriptionStatus <- getSubscriptionStatusForUser userName
      pure $
        UserInfo
          { userInfoUsername = userName,
            userInfoAdmin = mServerAdmin == Just userName,
            userInfoCreated = userCreated,
            userInfoLastLogin = userLastLogin,
            userInfoLastUse = userLastUse,
            userInfoSubscribed = subscriptionStatus
          }
