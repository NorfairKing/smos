{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Version
import Database.Persist.Sql as DB
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types as HTTP
import Path
import Paths_smos_sync_client
import Servant.Auth.Client as Auth
import Smos.API
import Smos.Client
import Smos.Sync.Client.Prompt
import Smos.Sync.Client.Session
import System.Exit
import UnliftIO

type C = ReaderT SyncClientEnv (LoggingT IO)

data SyncClientEnv = SyncClientEnv
  { syncClientEnvServantClientEnv :: ClientEnv,
    syncClientEnvConnection :: DB.ConnectionPool
  }
  deriving (Generic)

withClientEnv :: MonadIO m => BaseUrl -> (ClientEnv -> m a) -> m a
withClientEnv burl func = do
  let managerSets =
        HTTP.tlsManagerSettings
          { managerModifyRequest = \request -> do
              let headers =
                    ( "User-Agent",
                      TE.encodeUtf8 $ T.pack $ "smos-sync-client-" <> showVersion version
                    ) :
                    requestHeaders request
              pure $ request {requestHeaders = headers}
          }
  man <- liftIO $ HTTP.newManager managerSets
  let cenv = mkClientEnv man burl
  func cenv

withLogin ::
  (MonadUnliftIO m, MonadLogger m) =>
  ClientEnv ->
  Path Abs File ->
  Maybe Username ->
  Maybe Password ->
  (Auth.Token -> m a) ->
  m a
withLogin cenv sessionPath mun mpw func = do
  mToken <- loadToken sessionPath
  let loginAndPerformAction = do
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
  case mToken of
    Just token ->
      -- First we try to run the given action without login in again, if it
      -- fails because of a 401 error, we try to login and run the action again
      let predicate :: ClientError -> Maybe ()
          predicate = \case
            FailureResponse _ resp -> guard (responseStatusCode resp == HTTP.unauthorized401)
            _ -> Nothing
       in catchJust
            predicate
            (func token)
            ( \() -> do
                logWarnN "Request failed because of a 401 response, Will re-log-in and try again."
                loginAndPerformAction
            )
    Nothing -> loginAndPerformAction

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

runSyncClient :: NFData a => ClientM a -> C (Either ClientError a)
runSyncClient func = do
  cenv <- asks syncClientEnvServantClientEnv
  liftIO $ runClient cenv func

runSyncClientOrThrow :: NFData a => ClientM a -> C a
runSyncClientOrThrow func = do
  errOrResp <- runSyncClient func
  case errOrResp of
    Left err -> liftIO $ throwIO err
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
