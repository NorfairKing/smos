{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Assets where

import Smos.Docs.Site.Constants
import Yesod.EmbeddedStatic

mkEmbeddedStatic
  development
  "assets"
  [embedDir "content/assets"]
