{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Timestamps.Gen where

import Cursor.FuzzyLocalTime.Gen ()
import Cursor.Map.Gen
import Cursor.Text.Gen
import Data.GenValidity
import Smos.Cursor.Timestamps
import Smos.Data.Gen

instance GenValid TimestampsCursor where
  genValid =
    let tsnc = textCursorWithGen genTimestampNameChar
     in TimestampsCursor <$> genMapCursorBy tsnc genValid genValid genValid
  shrinkValid = shrinkValidStructurally
