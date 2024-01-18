{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Server.Env where

import Control.Concurrent
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import Data.Mergeful.Collection (ServerStore (..))
import Data.Mergeful.Timed (Timed (..))
import Data.Text (Text)
import Data.Word
import Database.Persist as DB
import Database.Persist.Sql as DB
import Database.Persist.Sqlite as DB
import GHC.Generics (Generic)
import Path
import Servant
import Servant.Auth.Server
import Smos.API
import Smos.Server.DB
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
  deriving (Generic)

runDB :: DB.SqlPersistT (LoggingT IO) a -> ServerHandler a
runDB func = do
  pool <- asks serverEnvConnection
  logFunc <- asks serverEnvLogFunc
  liftIO $ runLoggingT (DB.runSqlPool (DB.retryOnBusy func) pool) logFunc

readServerStore :: (MonadIO m) => UserId -> SqlPersistT m (Mergeful.ServerStore (Path Rel File) SyncFile)
readServerStore uid = do
  sfs <- selectList [ServerFileUser ==. uid] []
  pure $
    ServerStore $
      M.fromList $
        map
          ( \(Entity _ ServerFile {..}) ->
              ( serverFilePath,
                Timed
                  { timedValue =
                      SyncFile {syncFileContents = serverFileContents},
                    timedTime = serverFileTime
                  }
              )
          )
          sfs
