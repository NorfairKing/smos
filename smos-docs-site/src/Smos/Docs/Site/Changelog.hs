{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Smos.Docs.Site.Changelog
  ( module Smos.Docs.Site.Changelog,
    Changelog (..),
  )
where

import Data.Map (Map)
import Data.Text (Text)
import Language.Haskell.TH.Load
import Path
import Smos.Docs.Site.Changelog.TH
import Smos.Docs.Site.Constants

changelogs :: Load (Map [Text] Changelog)
changelogs = $$(embedTextFilesInWith changelogKeyFunc [||changelogKeyFunc||] changelogValFunc [||changelogValFunc||] mode [reldir|content/changelogs|])
