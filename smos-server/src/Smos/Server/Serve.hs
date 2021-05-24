{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Serve where

import qualified Codec.Compression.Zstd as Zstd (maxCLevel)
import Control.Monad.Logger
import Control.Monad.Reader
import Crypto.JOSE.JWK (JWK)
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON (encodePretty)
import qualified Data.ByteString.Lazy as LB
import Data.Function
import Data.Proxy
import qualified Data.Text as T
import Database.Persist.Sqlite as DB
import Lens.Micro
import Looper
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Path
import Path.IO
import Servant.API.Generic
import Servant.Auth.Server as Auth
import Servant.Server as Servant
import Servant.Server.Generic
import Smos.API
import Smos.Server.Constants
import Smos.Server.Handler
import Smos.Server.Looper
import System.Exit
import Text.Printf
import UnliftIO hiding (Handler)

serveSmosServer :: ServeSettings -> IO ()
serveSmosServer ss = do
  pPrint ss
  runSmosServer ss

runSmosServer :: ServeSettings -> IO ()
runSmosServer ServeSettings {..} = do
  ensureDir $ parent serveSetDatabaseFile
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= serveSetLogLevel) $
      DB.withSqlitePoolInfo (DB.mkSqliteConnectionInfo (T.pack $ fromAbsFile serveSetDatabaseFile) & DB.fkEnabled .~ False) 1 $
        \pool -> do
          flip DB.runSqlPool pool $ DB.runMigration serverAutoMigration
          let compressionLevel =
                if development
                  then 1 -- As fast as possible
                  else Zstd.maxCLevel -- rather slower
          logFunc <- askLoggerIO
          let runTheServer = do
                liftIO $ do
                  uuid <- readServerUUID serveSetUUIDFile
                  jwtKey <- loadSigningKey serveSetSigningKeyFile
                  priceVar <- newEmptyMVar
                  let env =
                        ServerEnv
                          { serverEnvServerUUID = uuid,
                            serverEnvConnection = pool,
                            serverEnvCookieSettings = defaultCookieSettings,
                            serverEnvJWTSettings = defaultJWTSettings jwtKey,
                            serverEnvPasswordDifficulty =
                              if development
                                then 4 -- As fast as possible
                                else 10, -- Rather slower
                            serverEnvLogFunc = logFunc,
                            serverEnvCompressionLevel = compressionLevel,
                            serverEnvMaxBackupsPerUser = serveSetMaxBackupsPerUser,
                            serverEnvMaxBackupSizePerUser = serveSetMaxBackupSizePerUser,
                            serverEnvAdmin = serveSetAdmin,
                            serverEnvPriceCache = priceVar,
                            serverEnvMonetisationSettings = serveSetMonetisationSettings
                          }
                  let middles =
                        if development
                          then Wai.logStdoutDev
                          else Wai.logStdout
                  Warp.run serveSetPort $ middles $ makeSyncApp env
          let runTheLoopers = do
                let looperEnv =
                      LooperEnv
                        { looperEnvConnection = pool,
                          looperEnvCompressionLevel = compressionLevel,
                          looperEnvMaxBackupsPerUser = serveSetMaxBackupsPerUser,
                          looperEnvBackupInterval = serveSetBackupInterval
                        }
                    looperRunner LooperDef {..} = do
                      logInfoNS looperDefName "Starting"
                      begin <- liftIO getCurrentTime
                      looperDefFunc
                      end <- liftIO getCurrentTime
                      logInfoNS looperDefName $ T.pack (printf "Done, took %.2f seconds" (realToFrac (diffUTCTime end begin) :: Double))
                flip runReaderT looperEnv $
                  runLoopersIgnoreOverrun
                    looperRunner
                    [ mkLooperDef "auto-backup" serveSetAutoBackupLooperSettings runAutoBackupLooper,
                      mkLooperDef "backup-garbage-collector" serveSetBackupGarbageCollectionLooperSettings runBackupGarbageCollectorLooper,
                      mkLooperDef "file-migrator" serveSetFileMigrationLooperSettings runFileMigrationLooper
                    ]
          concurrently_ runTheServer runTheLoopers

loadSigningKey :: Path Abs File -> IO JWK
loadSigningKey skf = do
  mErrOrKey <- forgivingAbsence $ JSON.eitherDecode <$> LB.readFile (toFilePath skf)
  case mErrOrKey of
    Nothing -> do
      key_ <- Auth.generateKey
      storeSigningKey skf key_
      pure key_
    Just (Left err) ->
      die $ unlines ["Failed to load signing key from file", fromAbsFile skf, "with error:", err]
    Just (Right r) -> pure r

storeSigningKey :: Path Abs File -> JWK -> IO ()
storeSigningKey skf key_ = do
  LB.writeFile (toFilePath skf) (JSON.encodePretty key_)

makeSyncApp :: ServerEnv -> Wai.Application
makeSyncApp env =
  let cfg = serverEnvCookieSettings env :. serverEnvJWTSettings env :. EmptyContext
      runServerHandler :: ServerHandler a -> Handler a
      runServerHandler func = runLoggingT (runReaderT func env) (serverEnvLogFunc env)
   in Servant.serveWithContext smosAPI cfg $
        hoistServerWithContext
          smosAPI
          (Proxy :: Proxy '[CookieSettings, JWTSettings])
          runServerHandler
          smosServantServer

smosServantServer :: ServerT SmosAPI ServerHandler
smosServantServer = toServant smosServerRecord

smosServerRecord :: APIRoutes (AsServerT ServerHandler)
smosServerRecord =
  APIRoutes
    { unprotectedRoutes = toServant syncServerUnprotectedRoutes,
      protectedRoutes = toServant syncServerProtectedRoutes,
      adminRoutes = toServant syncServerAdminRoutes
    }

syncServerUnprotectedRoutes :: UnprotectedRoutes (AsServerT ServerHandler)
syncServerUnprotectedRoutes =
  UnprotectedRoutes
    { getApiVersion = serveGetApiVersion,
      getMonetisation = serveGetMonetisation,
      postRegister = servePostRegister,
      postLogin = servePostLogin,
      postStripeHook = servePostStripeHook
    }

syncServerProtectedRoutes :: ProtectedRoutes (AsServerT ServerHandler)
syncServerProtectedRoutes =
  ProtectedRoutes
    { getUserPermissions = withAuthResult serveGetUserPermissions,
      getUserSubscription = withAuthResult serveGetUserSubscription,
      postInitiateStripeCheckoutSession = withAuthResult servePostInitiateStripeCheckoutSession,
      deleteUser = withAuthResult serveDeleteUser,
      postSync = withAuthResult servePostSync,
      getListBackups = withAuthResult serveGetListBackups,
      postBackup = withAuthResult servePostBackup,
      getBackup = withAuthResult serveGetBackup,
      putRestoreBackup = withAuthResult servePutRestoreBackup,
      deleteBackup = withAuthResult serveDeleteBackup,
      getListSmosFiles = withAuthResult serveGetListSmosFiles,
      getSmosFile = withAuthResult serveGetSmosFile,
      putSmosFile = withAuthResult servePutSmosFile,
      reportRoutes = toServant serverReportRoutes
    }

serverReportRoutes :: ReportRoutes (AsServerT ServerHandler)
serverReportRoutes =
  ReportRoutes
    { getNextActionReport = withAuthResult serveGetNextActionReport,
      getAgendaReport = withAuthResult serveGetAgendaReport
    }

syncServerAdminRoutes :: AdminRoutes (AsServerT ServerHandler)
syncServerAdminRoutes =
  AdminRoutes
    { getUsers = withAuthResult serveGetUsers,
      getUser = withAuthResult serveGetUser,
      putUserSubscription = withAuthResult servePutUserSubscription
    }

readServerUUID :: Path Abs File -> IO ServerUUID
readServerUUID p = do
  mContents <- forgivingAbsence $ LB.readFile $ fromAbsFile p
  case mContents of
    Nothing -> do
      u <- nextRandomUUID
      writeServerUUID p u
      pure u
    Just contents ->
      case JSON.eitherDecode contents of
        Left err -> die err
        Right u -> pure u

writeServerUUID :: Path Abs File -> ServerUUID -> IO ()
writeServerUUID p u = do
  ensureDir (parent p)
  LB.writeFile (fromAbsFile p) $ JSON.encodePretty u

withAuthResult :: ThrowAll a => (AuthNCookie -> a) -> (AuthResult AuthNCookie -> a)
withAuthResult func ar =
  case ar of
    Authenticated ac -> func ac
    _ -> throwAll err401
