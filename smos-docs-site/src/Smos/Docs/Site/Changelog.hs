{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Smos.Docs.Site.Changelog
  ( module Smos.Docs.Site.Changelog,
    Changelog (..),
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time
import Language.Haskell.TH.Load
import Path
import Smos.Docs.Site.Changelog.TH
import Smos.Docs.Site.Constants

unreleasedChangelog :: Load (Maybe Changelog)
unreleasedChangelog = $$(embedReadTextFileWith unreleasedFunc [||unreleasedFunc||] mode [relfile|content/unreleased.markdown|])

latestChangelog :: Load (Day, Changelog)
latestChangelog = M.findMin <$> changelogs

changelogs :: Load (Map Day Changelog)
changelogs = $$(embedTextFilesInWith changelogKeyFunc [||changelogKeyFunc||] changelogValFunc [||changelogValFunc||] mode [reldir|content/changelogs|])
