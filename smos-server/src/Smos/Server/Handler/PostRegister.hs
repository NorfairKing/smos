{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.PostRegister
  ( servePostRegister
  ) where

import Smos.Server.Handler.Import

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time

servePostRegister :: Register -> SyncHandler NoContent
servePostRegister Register {..} = do
  maybeHashedPassword <- liftIO $ passwordHash registerPassword
  case maybeHashedPassword of
    Nothing -> throwError err400 {errBody = "Failed to hash password."}
    Just hashedPassword -> do
      now <- liftIO getCurrentTime
      let user =
            User
              {userName = registerUsername, userHashedPassword = hashedPassword, userCreated = now}
      maybeUserEntity <- runDB . getBy $ UniqueUsername $ userName user
      case maybeUserEntity of
        Nothing -> runDB $ insert_ user
        Just _ ->
          throwError
            err409
              { errBody =
                  LB.fromStrict $
                  TE.encodeUtf8 $
                  T.unwords
                    ["Account with the username", usernameText registerUsername, "already exists."]
              }
  pure NoContent
