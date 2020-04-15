{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Properties.Gen where

import Cursor.Map.Gen
import Cursor.Text.Gen
import Data.GenValidity
import Smos.Cursor.Properties
import Smos.Data.Gen

instance GenValid PropertiesCursor where
  genValid =
    let pnc = textCursorWithGen genPropertyNameChar
        pvc = textCursorWithGen genPropertyValueChar
     in PropertiesCursor <$> genMapCursorBy pnc pvc genValid genValid
  shrinkValid = shrinkValidStructurally
