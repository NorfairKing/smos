{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Editor.Gen where

import Data.GenValidity

import Smos.Types

import Smos.Cursor.Help.Gen ()
import Smos.Cursor.SmosFile.Gen ()

instance GenUnchecked EditorCursor

instance GenValid EditorCursor where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked EditorSelection

instance GenValid EditorSelection where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
