{-# LANGUAGE DataKinds #-}

module Smos.Client
  ( module Smos.Client
  , module X
  ) where

import Servant.API.Flatten
import Servant.Auth.Server
import Servant.Client

import Smos.API as X

clientPostRegister :: Register -> ClientM NoContent
clientPostLogin ::
     Login
  -> ClientM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
clientPostRegister :<|> clientPostLogin = client (flatten syncUnprotectedAPI)

clientPostSync :: SyncRequest -> ClientM SyncResponse
clientPostSync = client (flatten syncProtectedAPI)
