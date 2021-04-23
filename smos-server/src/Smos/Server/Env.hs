{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Server.Env where

import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import Data.Mergeful.Collection (ServerStore (..))
import Data.Mergeful.Timed (Timed (..))
import Data.Word
import Database.Persist as DB
import Database.Persist.Sql as DB
import GHC.Generics (Generic)
import Path
import Servant
import Servant.Auth.Server
import Smos.API
import Smos.Server.DB

type ServerHandler = ReaderT ServerEnv Handler

data ServerEnv = ServerEnv
  { serverEnvServerUUID :: !ServerUUID,
    serverEnvConnection :: !DB.ConnectionPool,
    serverEnvCookieSettings :: !CookieSettings,
    serverEnvJWTSettings :: !JWTSettings,
    serverEnvPasswordDifficulty :: !Int,
    serverEnvCompressionLevel :: !Int, -- Between 1 and Codec.Compression.Zstd.maxCLevel
    serverEnvMaxBackupsPerUser :: !(Maybe Word),
    serverEnvMaxBackupSizePerUser :: !(Maybe Word64),
    serverEnvAdmin :: !(Maybe Username)
  }
  deriving (Generic)

runDB :: DB.SqlPersistT IO a -> ServerHandler a
runDB func = do
  pool <- asks serverEnvConnection
  liftIO $ DB.runSqlPool func pool

readServerStore :: MonadIO m => UserId -> SqlPersistT m (Mergeful.ServerStore (Path Rel File) SyncFile)
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
