module Main where

import Network.HTTP.Client.TLS
import Servant.Client
import Spec
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
  let makeClientEnv = do
        man <- newTlsManager
        bu <- parseBaseUrl su
        pure $ mkClientEnv man bu

  sydTest $ doNotRandomiseExecutionOrder $ sequential $ beforeAll makeClientEnv spec

newtype ServerUrl = ServerUrl String
