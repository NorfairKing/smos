{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Editor.Gen where

import Data.GenValidity

import Test.QuickCheck

import Smos.Cursor.Editor

import Smos.Cursor.SmosFile.Gen ()

instance GenUnchecked EditorCursor

instance GenValid EditorCursor where
    genValid = genValidStructurally

instance GenUnchecked EditorSelection

instance GenValid EditorSelection where
    genValid = genValidStructurally
