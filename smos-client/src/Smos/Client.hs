module Smos.Client
  ( module Smos.Client
  , module X
  ) where

import Servant.API.Flatten
import Servant.Client

import Smos.API as X

clientPostRegister :: Register -> ClientM NoContent
clientPostRegister = client (flatten syncUnprotectedAPI)

clientPostSync :: SyncRequest -> ClientM SyncResponse
clientPostSync = client (flatten syncProtectedAPI)
