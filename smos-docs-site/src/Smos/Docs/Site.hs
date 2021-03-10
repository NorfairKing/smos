{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Docs.Site
  ( smosDocsSite,
  )
where

import Smos.Docs.Site.Application ()
import Smos.Docs.Site.Foundation
import Smos.Docs.Site.OptParse
import Smos.Web.Style
import Yesod

smosDocsSite :: IO ()
smosDocsSite = do
  Instructions (DispatchServe ServeSettings {..}) Settings <- getInstructions
  Yesod.warp
    serveSetPort
    App
      { appAssets = assets,
        appCasts = casts,
        appStyle = smosWebStyle,
        appWebserverUrl = serveSetWebServerUrl,
        appGoogleAnalyticsTracking = serveSetGoogleAnalyticsTracking,
        appGoogleSearchConsoleVerification = serveSetGoogleSearchConsoleVerification
      }
