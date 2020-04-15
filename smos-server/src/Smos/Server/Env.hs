{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Server.Env where

import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import Data.Mergeful.Collection (ServerStore (..))
import Data.Mergeful.Timed (Timed (..))
import Database.Persist as DB
import Database.Persist.Sql as DB
import GHC.Generics (Generic)
import Servant
import Servant.Auth.Server
import Smos.API
import Smos.Server.DB

type SyncHandler = ReaderT ServerEnv Handler

data ServerEnv
  = ServerEnv
      { serverEnvServerUUID :: ServerUUID,
        serverEnvConnection :: DB.ConnectionPool,
        serverEnvCookieSettings :: CookieSettings,
        serverEnvJWTSettings :: JWTSettings
      }
  deriving (Generic)

runDB :: DB.SqlPersistT IO a -> SyncHandler a
runDB func = do
  pool <- asks serverEnvConnection
  liftIO $ DB.runSqlPool func pool

readServerStore :: MonadIO m => UserId -> SqlPersistT m (Mergeful.ServerStore FileUUID SyncFile)
readServerStore uid = do
  sfs <- selectList [ServerFileUser ==. uid] []
  pure
    $ ServerStore
    $ M.fromList
    $ map
      ( \(Entity _ ServerFile {..}) ->
          ( serverFileUuid,
            Timed
              { timedValue =
                  SyncFile {syncFilePath = serverFilePath, syncFileContents = serverFileContents},
                timedTime = serverFileTime
              }
          )
      )
      sfs

writeServerStore ::
  forall m.
  MonadIO m =>
  UserId ->
  Mergeful.ServerStore FileUUID SyncFile ->
  SqlPersistT m ()
writeServerStore uid ss = do
  deleteWhere [ServerFileUser ==. uid] -- Clean slate
  void $ M.traverseWithKey go $ serverStoreItems ss
  where
    go :: FileUUID -> Timed SyncFile -> SqlPersistT m ()
    go u Timed {..} =
      let SyncFile {..} = timedValue
       in void $
            upsertBy
              (UniqueServerFilePath uid syncFilePath)
              ( ServerFile
                  { serverFileUser = uid,
                    serverFileUuid = u,
                    serverFilePath = syncFilePath,
                    serverFileContents = syncFileContents,
                    serverFileTime = timedTime
                  }
              )
              [ServerFileContents =. syncFileContents, ServerFileTime =. timedTime]
