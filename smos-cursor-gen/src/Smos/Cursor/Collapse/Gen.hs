module Smos.Cursor.Collapse.Gen where

import Data.GenValidity

import Smos.Data.Gen ()

import Smos.Cursor.Collapse

instance GenUnchecked a => GenUnchecked (CollapseTree a)

instance GenValid a => GenValid (CollapseTree a) where
    genValid = genValidStructurally

instance GenUnchecked a => GenUnchecked (CollapseEntry a)

instance GenValid a => GenValid (CollapseEntry a) where
    genValid = genValidStructurally

instance GenUnchecked CollapseCycle
instance GenValid CollapseCycle
