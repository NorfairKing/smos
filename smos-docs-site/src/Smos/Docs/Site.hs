{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Docs.Site (smosDocsSite) where

import Control.Monad.Logger
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger as Wai
import Smos.CLI.Logging
import Smos.Docs.Site.Application ()
import Smos.Docs.Site.Constants
import Smos.Docs.Site.Foundation
import Smos.Docs.Site.OptParse
import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import System.Metrics.Prometheus.GHC.Stats as Prometheus (sampleGhcStats)
import System.Metrics.Prometheus.Wai.Middleware as Prometheus

smosDocsSite :: IO ()
smosDocsSite = do
  Settings {..} <- getSettings
  runFilteredLogger settingLogLevel $ do
    let app =
          App
            { appAssets = assets,
              appWebAssets = smosWebAssets,
              appWebserverUrl = settingWebServerUrl,
              appGoogleAnalyticsTracking = settingGoogleAnalyticsTracking,
              appGoogleSearchConsoleVerification = settingGoogleSearchConsoleVerification
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
    liftIO $ Warp.run settingPort $ middlewares plainApp
