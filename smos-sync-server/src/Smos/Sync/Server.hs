module Smos.Sync.Server where

import Smos.Sync.Server.OptParse
import Smos.Sync.Server.Serve

smosSyncServer :: IO ()
smosSyncServer = do
  Instructions dispatch Settings <- getInstructions
  case dispatch of
    DispatchServe ss -> serveSmosSyncServer ss
