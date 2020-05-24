{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.FileBrowser.Gen where

import Cursor.DirForest.Gen ()
import Data.GenValidity
import Smos.Cursor.FileBrowser

instance GenValid FileBrowserCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
