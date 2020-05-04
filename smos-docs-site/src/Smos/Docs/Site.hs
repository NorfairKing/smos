{-# LANGUAGE OverloadedStrings #-}

module Smos.Docs.Site
  ( smosDocsSite,
  )
where

import Smos.Docs.Site.Foundation
import Yesod

smosDocsSite :: IO ()
smosDocsSite =
  Yesod.warp 8000 App
