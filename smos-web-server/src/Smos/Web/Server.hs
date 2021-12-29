module Smos.Web.Server where

import Smos.Web.Server.OptParse
import Smos.Web.Server.Serve
import Text.Show.Pretty

smosWebServer :: IO ()
smosWebServer = do
  settings <- getSettings
  pPrint settings
  runSmosWebServer settings
