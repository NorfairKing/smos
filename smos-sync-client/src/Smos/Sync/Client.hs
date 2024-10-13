module Smos.Sync.Client
  ( smosSyncClient,
  )
where

import Smos.Sync.Client.Command
import Smos.Sync.Client.OptParse

smosSyncClient :: IO ()
smosSyncClient = do
  Instructions dispatch sets <- getInstructions
  case dispatch of
    DispatchRegister -> registerSmosSyncClient sets
    DispatchLogin -> loginSmosSyncClient sets
    DispatchSync ss -> syncSmosSyncClient sets ss
