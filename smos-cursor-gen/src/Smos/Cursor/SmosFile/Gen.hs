{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.SmosFile.Gen where

import Cursor.Forest.Gen ()
import Data.GenValidity
import Smos.Cursor.Collapse.Gen ()
import Smos.Cursor.Entry.Gen ()
import Smos.Cursor.SmosFile
import Smos.Data.Gen ()

instance GenValid SmosFileCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
