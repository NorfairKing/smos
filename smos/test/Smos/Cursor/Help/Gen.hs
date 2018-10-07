{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Help.Gen where

import Data.GenValidity

import Graphics.Vty.Input.Events

import Smos.Types

import Smos.Cursor.SmosFile.Gen ()

instance GenUnchecked Key

instance GenUnchecked Modifier

instance GenUnchecked KeyPress

instance GenValid KeyPress

instance GenUnchecked KeyCombination

instance GenValid KeyCombination

instance GenUnchecked HelpCursor

instance GenValid HelpCursor

instance GenUnchecked KeyHelpCursor

instance GenValid KeyHelpCursor where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
