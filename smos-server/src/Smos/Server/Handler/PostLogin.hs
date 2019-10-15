{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PostLogin where

import Smos.Server.Handler.Import

import qualified Data.Text.Encoding as TE

import Servant.Auth.Server

servePostLogin ::
     Login
  -> SyncHandler (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
servePostLogin Login {..} = do
  me <- runDB $ getBy $ UniqueUsername loginUsername
  case me of
    Nothing -> throwError err401
    Just (Entity _ user) ->
      if validatePassword (userHashedPassword user) loginPassword
        then setLoggedIn (userName user)
        else throwError err401
  where
    setLoggedIn un = do
      let cookie = AuthCookie {authCookieUsername = un}
      ServerEnv {..} <- ask
      mApplyCookies <- liftIO $ acceptLogin serverEnvCookieSettings serverEnvJWTSettings cookie
      case mApplyCookies of
        Nothing -> throwError err401
        Just applyCookies -> return $ applyCookies NoContent
