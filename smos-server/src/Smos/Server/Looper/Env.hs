{-# LANGUAGE DeriveGeneric #-}

module Smos.Server.Looper.Env where

import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Sql as DB
import GHC.Generics (Generic)

type Looper = ReaderT LooperEnv (LoggingT IO)

data LooperEnv = LooperEnv
  { looperEnvConnection :: !ConnectionPool,
    looperEnvCompressionLevel :: !Int,
    looperEnvMaxBackupsPerUser :: !(Maybe Word)
  }
  deriving (Generic)

looperDB :: DB.SqlPersistT (LoggingT IO) a -> Looper a
looperDB func = do
  pool <- asks looperEnvConnection
  lift $ DB.runSqlPool func pool
