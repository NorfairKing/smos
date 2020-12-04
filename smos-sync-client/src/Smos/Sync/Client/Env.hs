{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Client.Env where

import Control.DeepSeq
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Mergeful as Mergeful
import Data.Validity
import Database.Persist.Sql as DB
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Path
import Servant.Auth.Client as Auth
import Servant.Client as Servant
import Smos.API
import Smos.Client
import Smos.Sync.Client.Prompt
import Smos.Sync.Client.Session
import System.Exit

type C = ReaderT SyncClientEnv (LoggingT IO)

data SyncClientEnv = SyncClientEnv
  { syncClientEnvServantClientEnv :: Servant.ClientEnv,
    syncClientEnvConnection :: DB.ConnectionPool
  }
  deriving (Generic)

withClientEnv :: MonadIO m => BaseUrl -> (ClientEnv -> m a) -> m a
withClientEnv burl func = do
  man <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
  let cenv = mkClientEnv man burl
  func cenv

withLogin ::
  MonadIO m =>
  ClientEnv ->
  Path Abs File ->
  Maybe Username ->
  Maybe Password ->
  (Auth.Token -> m a) ->
  m a
withLogin cenv sessionPath mun mpw func = do
  mToken <- loadToken sessionPath
  case mToken of
    Just token -> func token
    Nothing -> do
      un <- liftIO $ promptUsername mun
      pw <- liftIO $ promptPassword mpw
      errOrErrOrSession <-
        liftIO $
          runClientOrDie cenv $
            clientLoginSession Login {loginUsername = un, loginPassword = unsafeShowPassword pw}
      case errOrErrOrSession of
        Left hp -> liftIO $ die $ unlines ["Problem with login headers:", show hp]
        Right cookie -> do
          saveSession sessionPath cookie
          func $ sessionToToken cookie

promptUsername :: Maybe Username -> IO Username
promptUsername mun =
  case mun of
    Nothing -> promptUntil "username" parseUsername
    Just un -> pure un

promptPassword :: Maybe Password -> IO Password
promptPassword mpw =
  case mpw of
    Nothing -> mkPassword <$> promptSecret "password"
    Just pw -> pure pw

runSyncClient :: Servant.ClientM a -> C (Either Servant.ClientError a)
runSyncClient func = do
  cenv <- asks syncClientEnvServantClientEnv
  liftIO $ Servant.runClientM func cenv

runSyncClientOrDie :: Servant.ClientM a -> C a
runSyncClientOrDie func = do
  errOrResp <- runSyncClient func
  case errOrResp of
    Left err -> liftIO $ die $ show err
    Right resp -> pure resp

runDB :: DB.SqlPersistT C a -> C a
runDB func = do
  pool <- asks syncClientEnvConnection
  DB.runSqlPool func pool

data ClientStore = ClientStore
  { clientStoreServerUUID :: ServerUUID,
    clientStoreItems :: Mergeful.ClientStore (Path Rel File) (Path Rel File) SyncFile
  }
  deriving (Show, Eq, Generic)

instance Validity ClientStore

instance NFData ClientStore

data SyncFileMeta = SyncFileMeta
  { syncFileMetaHash :: SHA256,
    syncFileMetaTime :: Mergeful.ServerTime
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity SyncFileMeta

instance NFData SyncFileMeta

instance FromJSON SyncFileMeta where
  parseJSON =
    withObject "SyncFileMeta" $ \o ->
      SyncFileMeta <$> o .: "sha256" <*> o .: "time"

instance ToJSON SyncFileMeta where
  toJSON SyncFileMeta {..} =
    object
      [ "sha256" .= syncFileMetaHash,
        "time" .= syncFileMetaTime
      ]
