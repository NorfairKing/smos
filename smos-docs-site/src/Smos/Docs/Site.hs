{-# LANGUAGE RecordWildCards #-}

module Smos.Docs.Site
  ( smosDocsSite,
  )
where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Smos.Docs.Site.Application ()
import Smos.Docs.Site.Constants
import Smos.Docs.Site.Foundation
import Smos.Docs.Site.OptParse
import Smos.Web.Style

smosDocsSite :: IO ()
smosDocsSite = do
  Instructions (DispatchServe ServeSettings {..}) Settings <- getInstructions
  let app =
        App
          { appAssets = assets,
            appCasts = casts,
            appStyle = smosWebStyle,
            appWebserverUrl = serveSetWebServerUrl,
            appGoogleAnalyticsTracking = serveSetGoogleAnalyticsTracking,
            appGoogleSearchConsoleVerification = serveSetGoogleSearchConsoleVerification
          }
  let defMiddles = defaultMiddlewaresNoLogging
  let extraMiddles =
        if development
          then Wai.logStdoutDev
          else Wai.logStdout
  let middle = extraMiddles . defMiddles
  plainApp <- liftIO $ toWaiAppPlain app
  let application = middle plainApp
  Warp.run serveSetPort application
