{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Changelog where

import Smos.Docs.Site.Changelog.TH
import Smos.Docs.Site.Changelog.Type

fullChangelog :: Changelog
fullChangelog = $(mkChangelog)
