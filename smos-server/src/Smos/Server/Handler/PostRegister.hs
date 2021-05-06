{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PostRegister
  ( servePostRegister,
  )
where

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Smos.Server.Handler.Import

servePostRegister :: Register -> ServerHandler NoContent
servePostRegister Register {..} = do
  difficulty <- asks serverEnvPasswordDifficulty
  hashedPassword <- liftIO $ hashPasswordWithParams difficulty $ mkPassword registerPassword
  now <- liftIO getCurrentTime
  let user =
        User
          { userName = registerUsername,
            userHashedPassword = hashedPassword,
            userCreated = now,
            userLastLogin = Nothing,
            userLastUse = Nothing
          }
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
