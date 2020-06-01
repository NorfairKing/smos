module Smos.Web.Server where

import Smos.Web.Server.OptParse
import Smos.Web.Server.Serve

smosWebServer :: IO ()
smosWebServer = do
  Instructions dispatch Settings <- getInstructions
  case dispatch of
    DispatchServe ss -> serveSmosWebServer ss
