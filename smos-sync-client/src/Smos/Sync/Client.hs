module Smos.Sync.Client
  ( smosSyncClient
  ) where

import Smos.Sync.Client.OptParse

import Smos.Sync.Client.Register
import Smos.Sync.Client.Sync

smosSyncClient :: IO ()
smosSyncClient = do
  Instructions dispatch sets <- getInstructions
  case dispatch of
    DispatchRegister rs -> registerSmosSyncClient sets rs
    DispatchSync ss -> syncSmosSyncClient sets ss
