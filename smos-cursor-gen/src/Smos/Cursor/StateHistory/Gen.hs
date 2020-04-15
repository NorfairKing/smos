{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.StateHistory.Gen where

import Cursor.Simple.List.NonEmpty.Gen ()
import Data.GenValidity
import Smos.Cursor.StateHistory
import Smos.Data.Gen ()
import Test.QuickCheck

instance GenValid StateHistoryCursor where
  genValid = genValid `suchThatMap` makeStateHistoryCursor -- TODO fix that this only makes cursors at a single possible selection
  shrinkValid = shrinkValidStructurally
