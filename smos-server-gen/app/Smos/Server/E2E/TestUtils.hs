{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.E2E.TestUtils where

import Data.Text (Text)
import Network.HTTP.Types as HTTP
import Smos.Client
import Test.Syd

testUsername :: Username
testUsername = Username "test-user"

testPassword :: Text
testPassword = "test-password"

testLogin :: Login
testLogin = Login {loginUsername = testUsername, loginPassword = testPassword}

cleanupTestUser :: ClientEnv -> IO ClientEnv
cleanupTestUser cenv = do
  errOrToken <- login cenv testLogin
  case errOrToken of
    Left err -> case err of
      LoginHeaderProblem err' -> expectationFailure $ "Unable to login for cleanup: " <> show err'
      LoginServantError ce -> case ce of
        FailureResponse _ resp ->
          if responseStatusCode resp == HTTP.unauthorized401
            then pure () -- We assume that it's because the account didn't exist
            else expectationFailure $ "Unable to login for cleanup: " <> show err
        _ -> expectationFailure $ "Unable to login for cleanup: " <> show err
    Right t -> runClientOrDie cenv $ do
      NoContent <- clientDeleteUser t
      pure ()
  pure cenv

withTestLogin cenv func = do
  errOrToken <-
    runClientOrDie cenv $
      clientLogin
        Login
          { loginUsername = testUsername,
            loginPassword = testPassword
          }
  case errOrToken of
    Left err -> expectationFailure $ show err
    Right t -> func t
