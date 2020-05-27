{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Undo.Gen where

import Cursor.List.Gen
import Data.GenValidity
import Smos.Undo

instance (GenValid a, GenValid b) => GenValid (Undo a b) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid a, GenValid b) => GenValid (UndoStack a b) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
