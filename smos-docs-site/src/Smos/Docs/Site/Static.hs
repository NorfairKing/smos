{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Static
  ( module Smos.Docs.Site.Static,
    DocPage (..),
  )
where

import Smos.Docs.Site.Static.TH

$mkPages
