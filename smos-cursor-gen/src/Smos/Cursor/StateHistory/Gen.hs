{-# OPTIONS_GHC -fno-warn-orphans #-}
module Smos.Cursor.StateHistory.Gen where

import Data.GenValidity

import Cursor.Simple.List.NonEmpty.Gen ()

import Smos.Data.Gen ()

import Smos.Cursor.StateHistory

instance GenUnchecked StateHistoryCursor

instance GenValid StateHistoryCursor where
    genValid = genValidStructurally
