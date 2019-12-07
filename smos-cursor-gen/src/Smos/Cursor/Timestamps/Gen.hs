{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Timestamps.Gen where

import Data.GenValidity

import Cursor.Map.Gen

import Smos.Data.Gen

import Cursor.FuzzyLocalTime.Gen ()
import Cursor.Text.Gen

import Smos.Cursor.Timestamps

instance GenValid TimestampsCursor where
  genValid =
    let tsnc = textCursorWithGen genTimestampNameChar
     in TimestampsCursor <$> genMapCursorBy tsnc genValid genValid genValid
  shrinkValid = shrinkValidStructurally
