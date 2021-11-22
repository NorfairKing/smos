{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Collapse.Gen where

import Data.GenValidity
import Smos.Cursor.Collapse
import Smos.Data.Gen ()

instance GenValid a => GenValid (CollapseEntry a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
