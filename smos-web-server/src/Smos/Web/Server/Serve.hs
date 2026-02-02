{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Web.Server.Serve where

import Control.Monad.Logger
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger as Wai
import Path.IO
import Paths_smos_web_server
import Servant.Client
import Smos.CLI.Logging
import Smos.Client
import Smos.Web.Assets
import Smos.Web.Server.Application ()
import Smos.Web.Server.Constants
import Smos.Web.Server.Foundation
import Smos.Web.Server.OptParse
import Smos.Web.Server.Static
import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import System.Metrics.Prometheus.GHC.Stats as Prometheus (sampleGhcStats)
import System.Metrics.Prometheus.Wai.Middleware as Prometheus
import Text.Show.Pretty (ppShow)
import Yesod

runSmosWebServer :: Settings -> IO ()
runSmosWebServer ss@Settings {..} = do
  runFilteredLogger settingLogLevel $ do
    logDebugN $ T.pack $ ppShow ss
    let managerSets =
          Http.tlsManagerSettings
            { Http.managerModifyRequest = \request -> do
                let headers =
                      ( "User-Agent",
                        TE.encodeUtf8 $ T.pack $ "smos-web-server-" <> showVersion version
                      )
                        :
                        -- TODO: Do this via yesod's 'getCurrentRoute' on a case-by-case basis
                        -- so that we have the exact path as well when we get `servant-client >=0.17`.
                        -- We can then also add the username to it.
                        -- http://hackage.haskell.org/package/yesod-core-1.6.19.0/docs/Yesod-Core-Handler.html#v:getCurrentRoute
                        ("Referer", TE.encodeUtf8 $ T.pack $ showBaseUrl settingWebUrl)
                        : Http.requestHeaders request
                pure $ request {Http.requestHeaders = headers}
            }
    man <- liftIO $ Http.newManager managerSets
    let cenv = mkClientEnv man settingAPIUrl
    sessionKeyFile <- liftIO $ resolveFile' "client_session_key.aes"
    let app =
          App
            { appLogLevel = settingLogLevel,
              appWebAssets = smosWebAssets,
              appStatic = smosWebServerStatic,
              appAPIClientEnv = cenv,
              appDocsBaseUrl = settingDocsUrl,
              appGoogleAnalyticsTracking = settingGoogleAnalyticsTracking,
              appGoogleSearchConsoleVerification = settingGoogleSearchConsoleVerification,
              appSessionKeyFile = sessionKeyFile
            }

    logFunc <- askLoggerIO
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
            . defaultMiddlewaresNoLogging

    plainApp <- liftIO $ toWaiAppPlain app
    liftIO $ withServerVersionCheck app $ Warp.run settingPort $ middlewares plainApp

-- | Check whether the smos-server version is supported.
--
-- This also checks whether the smos-server is online.
withServerVersionCheck :: App -> IO a -> IO a
withServerVersionCheck app func =
  withClientVersionCheck (appAPIClientEnv app) func
