{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Docs.Site
  ( smosDocsSite,
  )
where

import Smos.Docs.Site.Application ()
import Smos.Docs.Site.Foundation
import Smos.Docs.Site.OptParse
import Yesod

smosDocsSite :: IO ()
smosDocsSite = do
  Instructions (DispatchServe ServeSettings {..}) Settings <- getInstructions
  Yesod.warp
    serveSetPort
    App
      { appWebserverUrl = serveSetWebServerUrl,
        appAssets = assets,
        appCasts = casts
      }
