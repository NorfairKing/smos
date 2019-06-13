{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Help.Gen where

import Data.GenValidity

import Graphics.Vty.Input.Events

import Smos.Types

import Smos.Cursor.SmosFile.Gen ()
import Smos.Types.Gen ()

instance GenUnchecked Key

instance GenUnchecked Modifier

instance GenUnchecked KeyPress

instance GenValid KeyPress

instance GenUnchecked KeyCombination

instance GenValid KeyCombination

instance GenValid HelpCursor where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid KeyHelpCursor where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
