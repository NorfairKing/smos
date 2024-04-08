module Smos.Server.Looper.Env where

import Conduit
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Time
import Database.Persist.Sql as DB

type Looper = ReaderT LooperEnv (LoggingT IO)

data LooperEnv = LooperEnv
  { looperEnvConnection :: !ConnectionPool,
    looperEnvCompressionLevel :: !Int,
    looperEnvMaxBackupsPerPeriodPerUser :: ![(NominalDiffTime, Word)]
  }

looperDB :: DB.SqlPersistT (LoggingT IO) a -> Looper a
looperDB func = do
  pool <- asks looperEnvConnection
  lift $ DB.runSqlPool func pool

{-# ANN looperDBConduit ("NOCOVER" :: String) #-}
looperDBConduit :: ConduitT i o (DB.SqlPersistT (LoggingT IO)) a -> ConduitT i o Looper a
looperDBConduit = transPipe looperDB
