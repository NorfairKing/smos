{-# LANGUAGE OverloadedStrings #-}

module Smos.Docs.Site
  ( smosDocsSite,
  )
where

import Smos.Docs.Site.Application ()
import Smos.Docs.Site.Assets
import Smos.Docs.Site.Foundation
import System.Environment
import Yesod

smosDocsSite :: IO ()
smosDocsSite = do
  port <- read <$> getEnv "SMOS_DOCS_SITE_PORT"
  Yesod.warp port App {appAssets = assets}
