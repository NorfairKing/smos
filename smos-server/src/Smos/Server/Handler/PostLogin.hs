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
  ServerHandler (Headers '[Header "Set-Cookie" T.Text] NoContent)
servePostLogin Login {..} = do
  me <- runDB $ getBy $ UniqueUsername loginUsername
  case me of
    Nothing -> throwError err401
    Just e@(Entity _ user) ->
      if development
        then setLoggedIn e
        else case checkPassword (mkPassword loginPassword) (userHashedPassword user) of
          PasswordCheckSuccess -> setLoggedIn e
          PasswordCheckFail -> throwError err401
  where
    setLoggedIn (Entity uid User {..}) = do
      let cookie =
            AuthCookie
              { authCookieUsername = userName
              }
      ServerEnv {..} <- ask
      mCookie <- liftIO $ makeSessionCookieBS serverEnvCookieSettings serverEnvJWTSettings cookie
      case mCookie of
        Nothing -> throwError err401
        Just setCookie -> do
          now <- liftIO getCurrentTime
          runDB $ update uid [UserLastLogin =. Just now]
          pure $ addHeader (decodeUtf8 setCookie) NoContent
