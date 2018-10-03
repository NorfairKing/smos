{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.StateHistory.Gen where

import Data.GenValidity

import Test.QuickCheck

import Cursor.Simple.List.NonEmpty.Gen ()

import Smos.Data.Gen ()

import Smos.Cursor.StateHistory

instance GenUnchecked StateHistoryCursor

instance GenValid StateHistoryCursor where
    genValid = genValid `suchThatMap` makeStateHistoryCursor -- TODO fix that this only makes cursors at a single possible selection
    shrinkValid = shrinkValidStructurally
