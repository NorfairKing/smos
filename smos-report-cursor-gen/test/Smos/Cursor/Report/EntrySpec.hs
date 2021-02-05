{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.EntrySpec where

import Smos.Cursor.Report.Entry
import Smos.Cursor.Report.Entry.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Lens

spec :: Spec
spec = do
  genValidSpec @(EntryReportCursor ())
  genValidSpec @EntryReportCursorSelection
  genValidSpec @(EntryReportEntryCursor ())
  describe "entryReportCursorEntryEntryCursorsL" $ lensSpecOnValid (entryReportCursorEntryReportEntryCursorsL @())
  describe "entryReportCursorSelectedEntryEntryCursorsL" $ lensSpecOnValid (entryReportCursorSelectedEntryReportEntryCursorsL @())
  describe "entryReportCursorSelectionL" $ lensSpecOnValid (entryReportCursorSelectionL @())
  describe "entryReportCursorFilterBarL" $ lensSpecOnValid (entryReportCursorFilterBarL @())
  describe "makeEntryEntryCursor" $ it "produces valid cursors" $ producesValidsOnValids3 (makeEntryReportEntryCursor @())
