{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Entry.Gen where

import Cursor.Forest
import Cursor.Forest.Gen ()
import Cursor.Text.Gen ()
import Cursor.Tree
import Data.GenValidity
import Data.GenValidity.Path ()
import Data.Maybe
import Lens.Micro
import Path
import Smos.Cursor.Report.Entry
import Smos.Data
import Smos.Data.Gen ()
import Test.QuickCheck

instance GenValid a => GenValid (EntryReportCursor a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid EntryReportCursorSelection where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid a => GenValid (EntryReportEntryCursor a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

genValidEntryReportCursorWith ::
  (Path Rel File -> ForestCursor Entry Entry -> [a]) ->
  ([EntryReportEntryCursor a] -> [EntryReportEntryCursor a]) ->
  Gen Entry ->
  Gen (EntryReportCursor a)
genValidEntryReportCursorWith func finalise gen = makeEntryReportCursor . finalise <$> genListOf (genValidEntryReportEntryCursorWith func gen)

genValidEntryReportEntryCursorWith :: (Path Rel File -> ForestCursor Entry Entry -> [a]) -> Gen Entry -> Gen (EntryReportEntryCursor a)
genValidEntryReportEntryCursorWith func gen =
  ( do
      rf <- genValid
      fc <- genValid
      e <- gen
      pure (rf, fc & forestCursorSelectedTreeL . treeCursorCurrentL .~ e)
  )
    `suchThatMap` ( \(rf, fc) -> do
                      val <- listToMaybe (func rf fc)
                      pure $ makeEntryReportEntryCursor rf fc val
                  )
