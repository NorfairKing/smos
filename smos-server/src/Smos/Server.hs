module Smos.Server where

import Smos.Server.OptParse
import Smos.Server.Serve

smosServer :: IO ()
smosServer = do
  Instructions dispatch Settings <- getInstructions
  case dispatch of
    DispatchServe ss -> serveSmosServer ss
