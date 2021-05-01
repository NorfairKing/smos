{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Client.Command.Login where

import Control.Monad.Logger
import Smos.Client
import Smos.Sync.Client.Env
import Smos.Sync.Client.OptParse

loginSmosSyncClient :: Settings -> LoginSettings -> IO ()
loginSmosSyncClient Settings {..} LoginSettings =
  withClientEnv setServerUrl $ \cenv ->
    withClientVersionCheck cenv $
      runStderrLoggingT $
        filterLogger (\_ ll -> ll >= setLogLevel) $
          withLogin cenv setSessionPath setUsername setPassword (const $ pure ())
