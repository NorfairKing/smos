{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Help.Gen where

import Data.GenValidity

import Smos.Types

import Smos.Cursor.SmosFile.Gen ()
import Smos.Keys.Gen ()
import Smos.Types.Gen ()

instance GenValid KeyCombination where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid HelpCursor where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid HelpCursorSelection where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid KeyHelpCursor where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
