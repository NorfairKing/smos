{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Client.Command.Login where

import Smos.CLI.Logging
import Smos.Client
import Smos.Sync.Client.Env
import Smos.Sync.Client.OptParse

loginSmosSyncClient :: Settings -> LoginSettings -> IO ()
loginSmosSyncClient Settings {..} LoginSettings =
  withClientEnv setServerUrl $ \cenv ->
    withClientVersionCheck cenv $
      runFilteredLogger setLogLevel $
        withLogin cenv setSessionPath setUsername setPassword (const $ pure ())
