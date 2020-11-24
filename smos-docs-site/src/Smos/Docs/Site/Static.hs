{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Smos.Docs.Site.Static
  ( module Smos.Docs.Site.Static,
    DocPage (..),
  )
where

import Path
import Smos.Docs.Site.Static.TH

$mkDocPages
