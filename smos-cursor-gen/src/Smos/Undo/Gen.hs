{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Undo.Gen where

import Cursor.List.Gen ()
import Data.GenValidity
import Smos.Undo

instance (GenValid a) => GenValid (UndoStack a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
