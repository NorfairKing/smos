{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Smos.Docs.Site.Static
  ( module Smos.Docs.Site.Static,
    DocPage (..),
  )
where

import Data.Map (Map)
import Data.Text (Text)
import Language.Haskell.TH.Load
import Path
import Smos.Docs.Site.Constants
import Smos.Docs.Site.Static.TH

docPages :: Load (Map [Text] DocPage)
docPages = $$(embedTextFilesInWith docPageKeyFunc [||docPageKeyFunc||] docPageValFunc [||docPageValFunc||] mode [reldir|content/pages|])
