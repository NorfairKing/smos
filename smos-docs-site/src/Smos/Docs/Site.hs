{-# LANGUAGE OverloadedStrings #-}

module Smos.Docs.Site
  ( smosDocsSite,
  )
where

import Smos.Docs.Site.Application ()
import Smos.Docs.Site.Assets
import Smos.Docs.Site.Foundation
import System.Environment
import System.Exit
import Text.Read
import Yesod

smosDocsSite :: IO ()
smosDocsSite = do
  portVar <- lookupEnv "SMOS_DOCS_SITE_PORT"
  port <- case portVar of
    Nothing -> pure 8000
    Just s -> case readMaybe s of
      Nothing -> die "Unable to read port environment variable."
      Just p -> pure p
  Yesod.warp port App {appAssets = assets}
