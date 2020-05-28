{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.FileBrowser.Gen where

import Cursor.DirForest.Gen ()
import Data.GenValidity
import Smos.Cursor.FileBrowser
import Smos.Data.Gen ()
import Smos.Undo.Gen ()

instance GenValid FileBrowserCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid FileBrowserCursorAction where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
