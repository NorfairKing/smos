{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.SmosFileEditor.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Cursor.Collapse.Gen ()
import Smos.Cursor.Entry.Gen ()
import Smos.Cursor.SmosFile.Gen ()
import Smos.Cursor.SmosFileEditor
import Smos.Data.Gen ()
import Smos.History.Gen ()

instance GenValid SmosFileEditorCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
