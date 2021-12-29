module Smos.Server where

import Smos.Server.OptParse
import Smos.Server.Serve

smosServer :: IO ()
smosServer = do
  settings <- getSettings
  serveSmosServer settings
