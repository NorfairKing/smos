module Main where

import Network.HTTP.Client.TLS
import Servant.Client
import Smos.Client
import Smos.Server.E2E.LoginSpec
import Smos.Server.E2E.VersionSpec
import System.Environment
import System.Exit
import Test.Syd

main :: IO ()
main = do
  let varname = "SMOS_SERVER_URL"
  me <- lookupEnv varname
  su <- case me of
    Nothing -> die $ varname <> " is not configured. Point it to the server to test. Example: api.testing.smos.online"
    Just su -> pure su

  man <- newTlsManager
  bu <- parseBaseUrl su
  let cenv = mkClientEnv man bu

  serverVersion <- runClientOrDie cenv clientGetApiVersion

  sydTest $
    doNotRandomiseExecutionOrder $
      sequential $
        beforeAll (pure cenv) $ do
          Smos.Server.E2E.LoginSpec.spec serverVersion
          Smos.Server.E2E.VersionSpec.spec

newtype ServerUrl = ServerUrl String
