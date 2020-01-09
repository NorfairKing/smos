{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Entry.Gen where

import Data.GenValidity

import Test.QuickCheck

import Cursor.FuzzyLocalTime.Gen ()

import Smos.Cursor.Entry

import Smos.Cursor.Contents.Gen ()
import Smos.Cursor.Header.Gen ()
import Smos.Cursor.Logbook.Gen ()
import Smos.Cursor.Properties.Gen ()
import Smos.Cursor.StateHistory.Gen ()
import Smos.Cursor.Tags.Gen ()
import Smos.Cursor.Timestamps.Gen ()

instance GenValid EntryCursor where
  genValid =
    sized $ \n -> do
      (a, b, c, d, e, f, g) <- genSplit7 n
      entryCursorHeaderCursor <- resize a genValid
      entryCursorContentsCursor <- resize b genValid
      entryCursorTimestampsCursor <- resize c genValid
      entryCursorPropertiesCursor <- resize d genValid
      entryCursorStateHistoryCursor <- resize e genValid
      entryCursorTagsCursor <- resize f genValid
      entryCursorLogbookCursor <- resize g genValid
      entryCursorSelected <- genValid -- Doesn't care about size
      pure EntryCursor {..}
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid EntryCursorSelection where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
