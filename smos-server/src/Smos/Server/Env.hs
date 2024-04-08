{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Server.Env where

import Conduit
import Control.Concurrent
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import Data.Word
import Database.Persist.Sql as DB
import Database.Persist.Sqlite as DB
import Servant
import Servant.Auth.Server
import Smos.API
import Smos.Server.OptParse.Types
import StripeClient as Stripe

type ServerHandler = ReaderT ServerEnv (LoggingT Handler)

data ServerEnv = ServerEnv
  { serverEnvServerUUID :: !ServerUUID,
    serverEnvConnection :: !DB.ConnectionPool,
    serverEnvCookieSettings :: !CookieSettings,
    serverEnvJWTSettings :: !JWTSettings,
    serverEnvPasswordDifficulty :: !Int,
    serverEnvCompressionLevel :: !Int, -- Between 1 and Codec.Compression.Zstd.maxCLevel
    serverEnvLogFunc :: !(Loc -> LogSource -> LogLevel -> LogStr -> IO ()),
    serverEnvMaxBackupSizePerUser :: !(Maybe Word64),
    serverEnvAdmin :: !(Maybe Username),
    serverEnvBookingEmailAddress :: !(Maybe Text),
    serverEnvPriceCache :: !(MVar Stripe.Price), -- Indefinite cache, so an MVar works.
    serverEnvMonetisationSettings :: !(Maybe MonetisationSettings)
  }

runDB :: DB.SqlPersistT (LoggingT IO) a -> ServerHandler a
runDB func = do
  pool <- asks serverEnvConnection
  logFunc <- asks serverEnvLogFunc
  liftIO $ runLoggingT (DB.runSqlPool (DB.retryOnBusy func) pool) logFunc

{-# ANN runDBConduit ("NOCOVER" :: String) #-}
runDBConduit :: ConduitT i o (DB.SqlPersistT (LoggingT IO)) r -> ConduitT i o ServerHandler r
runDBConduit = transPipe runDB
