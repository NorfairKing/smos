{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Sync.Client.Env where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Timed as Mergeful
import Data.Validity

import System.Exit

import Servant.Client as Servant

import Path

import Control.Monad.Logger
import Control.Monad.Reader

import Database.Persist.Sql as DB

import Smos.Sync.API

import Smos.Sync.Client.DB

type C = ReaderT SyncClientEnv (LoggingT IO)

data SyncClientEnv =
  SyncClientEnv
    { syncClientEnvServantClientEnv :: Servant.ClientEnv
    , syncClientEnvConnection :: DB.ConnectionPool
    }
  deriving (Generic)

runClient :: Servant.ClientM a -> C (Either Servant.ClientError a)
runClient func = do
  cenv <- asks syncClientEnvServantClientEnv
  liftIO $ Servant.runClientM func cenv

runClientOrDie :: Servant.ClientM a -> C a
runClientOrDie func = do
  errOrResp <- runClient func
  case errOrResp of
    Left err -> liftIO $ die $ show err
    Right resp -> pure resp

runDB :: DB.SqlPersistT IO a -> C a
runDB func = do
  pool <- asks syncClientEnvConnection
  liftIO $ DB.runSqlPool func pool

readClientMetadata :: MonadIO m => SqlPersistT m (Map (Path Rel File) SyncFileMeta)
readClientMetadata = do
  cfs <- selectList [] []
  pure $
    M.fromList $
    map
      (\(Entity _ ClientFile {..}) ->
         ( clientFilePath
         , SyncFileMeta
             { syncFileMetaUUID = clientFileUuid
             , syncFileMetaHash = clientFileHash
             , syncFileMetaTime = clientFileTime
             }))
      cfs

writeClientMetadata ::
     forall m. MonadIO m
  => Map (Path Rel File) SyncFileMeta
  -> SqlPersistT m ()
writeClientMetadata m = do
  deleteWhere [ClientFilePath /<-. M.keys m]
  void $ M.traverseWithKey go m
  where
    go :: Path Rel File -> SyncFileMeta -> SqlPersistT m ()
    go path SyncFileMeta {..} =
      void $
      upsertBy
        (UniquePath path)
        (ClientFile
           { clientFileUuid = syncFileMetaUUID
           , clientFilePath = path
           , clientFileHash = syncFileMetaHash
           , clientFileTime = syncFileMetaTime
           })
        [ClientFileHash =. syncFileMetaHash, ClientFileTime =. syncFileMetaTime]

data ClientStore =
  ClientStore
    { clientStoreServerUUID :: ServerUUID
    , clientStoreItems :: Mergeful.ClientStore FileUUID SyncFile
    }
  deriving (Show, Eq, Generic)

instance Validity ClientStore

data SyncFileMeta =
  SyncFileMeta
    { syncFileMetaUUID :: FileUUID
    , syncFileMetaHash :: Int
    , syncFileMetaTime :: Mergeful.ServerTime
    }
  deriving (Show, Eq, Generic)

instance Validity SyncFileMeta

instance FromJSON SyncFileMeta where
  parseJSON =
    withObject "SyncFileMeta" $ \o -> SyncFileMeta <$> o .: "uuid" <*> o .: "hash" <*> o .: "time"

instance ToJSON SyncFileMeta where
  toJSON SyncFileMeta {..} =
    object ["uuid" .= syncFileMetaUUID, "hash" .= syncFileMetaHash, "time" .= syncFileMetaTime]
