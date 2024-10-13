{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Client.Command.Register where

import Smos.Client
import Smos.Sync.Client.Env
import Smos.Sync.Client.OptParse

registerSmosSyncClient :: Settings -> IO ()
registerSmosSyncClient Settings {..} =
  withClientEnv setServerUrl $ \cenv -> do
    un <- promptUsername setUsername
    pw <- promptPassword setPassword
    let reg = Register {registerUsername = un, registerPassword = unsafeShowPassword pw}
    NoContent <- runClientOrDie cenv $ clientWithVersionCheck $ clientPostRegister reg
    pure ()
