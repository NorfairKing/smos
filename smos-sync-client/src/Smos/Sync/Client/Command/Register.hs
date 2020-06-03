{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Client.Command.Register where

import Smos.Client
import Smos.Sync.Client.Env
import Smos.Sync.Client.OptParse

registerSmosSyncClient :: Settings -> RegisterSettings -> IO ()
registerSmosSyncClient Settings {..} RegisterSettings =
  withClientEnv setServerUrl $ \cenv -> do
    un <- promptUsername setUsername
    pw <- promptPassword setPassword
    let reg = Register {registerUsername = un, registerPassword = pw}
    NoContent <- runClientOrDie cenv $ clientPostRegister reg
    pure ()
