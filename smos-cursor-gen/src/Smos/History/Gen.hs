{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.History.Gen where

import Cursor.List.NonEmpty.Gen ()
import Data.GenValidity
import Smos.History

instance (GenValid s) => GenValid (History s) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
