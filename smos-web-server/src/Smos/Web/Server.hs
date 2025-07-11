module Smos.Web.Server where

import Smos.Web.Server.OptParse
import Smos.Web.Server.Serve

smosWebServer :: IO ()
smosWebServer = getSettings >>= runSmosWebServer
