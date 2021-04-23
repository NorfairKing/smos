{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PostLogin where

import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Servant.Auth.Server
import Smos.Server.Handler.Import

servePostLogin ::
  Login ->
  ServerHandler (Headers '[Header "Set-Cookie" T.Text] UserInfo)
servePostLogin Login {..} = do
  me <- runDB $ getBy $ UniqueUsername loginUsername
  case me of
    Nothing -> throwError err401
    Just (Entity _ user) ->
      let un = userName user
       in if development
            then setLoggedIn un
            else case checkPassword (mkPassword loginPassword) (userHashedPassword user) of
              PasswordCheckSuccess -> setLoggedIn un
              PasswordCheckFail -> throwError err401
  where
    setLoggedIn un = do
      mAdmin <- asks serverEnvAdmin
      let isAdmin = mAdmin == Just un
      let cookie =
            AuthCookie
              { authCookieUsername = un,
                authCookieIsAdmin = isAdmin
              }
      ServerEnv {..} <- ask
      mCookie <- liftIO $ makeSessionCookieBS serverEnvCookieSettings serverEnvJWTSettings cookie
      case mCookie of
        Nothing -> throwError err401
        Just setCookie -> do
          let ui = UserInfo {userInfoUsername = un, userInfoAdmin = isAdmin}
          pure $ addHeader (decodeUtf8 setCookie) ui
