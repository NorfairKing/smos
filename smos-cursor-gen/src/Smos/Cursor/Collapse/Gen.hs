{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Collapse.Gen where

import Data.GenValidity

import Smos.Data.Gen ()

import Smos.Cursor.Collapse

instance GenUnchecked a => GenUnchecked (CollapseEntry a)

instance GenValid a => GenValid (CollapseEntry a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
