{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Undo.Gen where

import Cursor.List.Gen
import Data.GenValidity
import Smos.Undo
import Test.QuickCheck

instance (GenValid a, GenValid b) => GenValid (Undo a b) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

genUndoDependent :: Gen (a, b) -> Gen (Undo a b)
genUndoDependent = fmap (uncurry Undo)

instance (GenValid a, GenValid b) => GenValid (UndoStack a b) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

genUndoStackDependent :: Gen (a, b) -> Gen (UndoStack a b)
genUndoStackDependent = fmap UndoStack . listCursorWithGen . genUndoDependent
