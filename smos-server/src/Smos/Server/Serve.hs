{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Server.Serve where

import Codec.Compression.Zstd.Extended as Zstd (defaultCLevel)
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Function
import Data.Proxy
import qualified Data.Text as T
import Database.Persist.Sqlite as DB
import Lens.Micro
import Looper
import Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Path
import Path.IO
import Servant.API.Generic
import Servant.Auth.Server as Auth
import Servant.Server as Servant
import Servant.Server.Generic
import Smos.API
import Smos.CLI.Logging
import Smos.Server.Constants
import Smos.Server.Handler
import Smos.Server.Looper
import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import System.Metrics.Prometheus.GHC.Stats as Prometheus (sampleGhcStats)
import System.Metrics.Prometheus.Wai.Middleware as Prometheus
import Text.Printf
import UnliftIO hiding (Handler)

serveSmosServer :: Settings -> IO ()
serveSmosServer ss@Settings {..} = do
  ensureDir $ parent settingDatabaseFile
  runFilteredLogger settingLogLevel $
    DB.withSqlitePoolInfo (DB.mkSqliteConnectionInfo (T.pack $ fromAbsFile settingDatabaseFile) & DB.fkEnabled .~ False) 1 $ \pool -> do
      logDebugN $ T.pack $ ppShow ss
      flip DB.runSqlPool pool $ completeServerMigration False
      let compressionLevel =
            if development
              then 1 -- As fast as possible
              else Zstd.defaultCLevel -- rather slower
      logFunc <- askLoggerIO
      let runTheServer = do
            liftIO $ do
              priceVar <- newEmptyMVar
              let env =
                    ServerEnv
                      { serverEnvServerUUID = settingUUID,
                        serverEnvConnection = pool,
                        serverEnvCookieSettings = defaultCookieSettings,
                        serverEnvJWTSettings = defaultJWTSettings settingSigningKey,
                        serverEnvPasswordDifficulty =
                          if development
                            then 4 -- As fast as possible
                            else 10, -- Rather slower
                        serverEnvLogFunc = logFunc,
                        serverEnvCompressionLevel = compressionLevel,
                        serverEnvMaxBackupSizePerUser = settingMaxBackupSizePerUser,
                        serverEnvAdmin = settingAdmin,
                        serverEnvBookingEmailAddress = settingBookingEmailAddress,
                        serverEnvPriceCache = priceVar,
                        serverEnvMonetisationSettings = settingMonetisationSettings
                      }
              loggingMiddleware <-
                liftIO $
                  mkRequestLogger
                    defaultRequestLoggerSettings
                      { destination = Callback $ \str ->
                          logFunc defaultLoc "warp" LevelInfo str,
                        outputFormat =
                          if development
                            then Detailed True
                            else Apache FromSocket
                      }
              registry <- liftIO Registry.new
              waiMetrics <- liftIO $ registerWaiMetrics mempty registry
              metricsEndpoint <-
                metricsEndpointMiddleware $
                  Prometheus.withLastSecondSamples (sampleGhcStats mempty) $
                    defaultMetricsEndpoint registry
              let middlewares =
                    metricsEndpoint
                      . instrumentWaiMiddleware waiMetrics
                      . loggingMiddleware

              Warp.run settingPort $ middlewares $ makeServerApp env
      let runTheLoopers = do
            let looperEnv =
                  LooperEnv
                    { looperEnvConnection = pool,
                      looperEnvCompressionLevel = compressionLevel,
                      looperEnvMaxBackupsPerPeriodPerUser = settingMaxBackupsPerPeriodPerUser
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
                [ mkLooperDef "auto-backup" settingAutoBackupLooperSettings runAutoBackupLooper,
                  mkLooperDef "backup-garbage-collector" settingBackupGarbageCollectionLooperSettings runBackupGarbageCollectorLooper
                ]
      concurrently_ runTheServer runTheLoopers

makeServerApp :: ServerEnv -> Wai.Application
makeServerApp env =
  let cfg = serverEnvCookieSettings env :. serverEnvJWTSettings env :. EmptyContext
   in Servant.serveWithContext smosAPI cfg (smosBaseServantServer env)

{-# ANN smosBaseServantServer ("NOCOVER" :: String) #-}
smosBaseServantServer :: ServerEnv -> Server SmosAPI
smosBaseServantServer env =
  let runServerHandler :: ServerHandler a -> Handler a
      runServerHandler func = runLoggingT (runReaderT func env) (serverEnvLogFunc env)
   in hoistServerWithContext
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
      postStripeHook = servePostStripeHook,
      getBookingSettings = serveGetBookingSettings,
      getBookingSlots = serveGetBookingSlots,
      postBooking = servePostBooking
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
      deleteSmosFile = withAuthResult serveDeleteSmosFile,
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
    { postMigrateFiles = withAuthResult servePostMigrateFiles,
      getUsers = withAuthResult serveGetUsers,
      getUser = withAuthResult serveGetUser,
      putUserSubscription = withAuthResult servePutUserSubscription
    }

withAuthResult :: (ThrowAll a) => (AuthNCookie -> a) -> (AuthResult AuthNCookie -> a)
withAuthResult func ar =
  case ar of
    Authenticated ac -> func ac
    _ -> throwAll err401
