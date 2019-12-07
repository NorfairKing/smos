{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Properties.Gen where

import Data.GenValidity

import Cursor.List.NonEmpty
import Cursor.Map
import Cursor.Map.Gen
import Cursor.Text.Gen

import Control.Monad

import Test.QuickCheck

import Smos.Data.Gen

import Smos.Cursor.Properties

instance GenValid PropertiesCursor where
  genValid =
    let pnc = textCursorWithGen genPropertyNameChar
        pvc = textCursorWithGen genPropertyValueChar
     in PropertiesCursor <$> genMapCursorBy pnc pvc genValid genValid
  shrinkValid = shrinkValidStructurally
