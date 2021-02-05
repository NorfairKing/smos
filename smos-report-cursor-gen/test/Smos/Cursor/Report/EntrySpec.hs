{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.EntrySpec where

import Smos.Cursor.Report.Entry
import Smos.Cursor.Report.Entry.Gen ()
import Smos.Report.TestUtils
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Lens

spec :: Spec
spec = do
  genValidSpec @EntryReportCursor
  genValidSpec @EntryReportCursorSelection
  genValidSpec @EntryReportEntryCursor
  describe "entryReportCursorEntryEntryCursorsL" $ lensSpecOnValid entryReportCursorEntryReportEntryCursorsL
  describe "entryReportCursorSelectedEntryEntryCursorsL" $ lensSpecOnValid entryReportCursorSelectedEntryReportEntryCursorsL
  describe "entryReportCursorSelectionL" $ lensSpecOnValid entryReportCursorSelectionL
  describe "entryReportCursorFilterBarL" $ lensSpecOnValid entryReportCursorFilterBarL
  describe "makeEntryEntryCursor" $ it "produces valid cursors" $ producesValidsOnValids2 makeEntryReportEntryCursor
  modifyMaxSuccess (`div` 10) $
    describe "produceEntryReportCursor" $
      it "produces valid reports for interesting stores" $
        withInterestingStore $
          \dc -> do
            narc <- produceEntryReportCursor dc
            shouldBeValid narc
