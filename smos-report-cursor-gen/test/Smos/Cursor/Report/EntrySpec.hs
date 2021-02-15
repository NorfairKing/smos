{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.EntrySpec where

import Smos.Cursor.Report.Entry
import Smos.Cursor.Report.Entry.Gen ()
import Test.QuickCheck
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
  describe "entryReportCursorNext" $ do
    it "produces valid cursors" $
      producesValidsOnValids @(EntryReportCursor ())
        entryReportCursorNext
    it "fails when there are no selected entries" $
      forAll ((\erc -> erc {entryReportCursorSelectedEntryReportEntryCursors = Nothing}) <$> genValid) $ \erc ->
        entryReportCursorNext @(EntryReportCursor ()) erc `shouldBe` Nothing
  describe "entryReportCursorPrev" $ do
    it "produces valid cursors" $ producesValidsOnValids @(EntryReportCursor ()) entryReportCursorPrev
    it "fails when there are no selected entries" $
      forAll ((\erc -> erc {entryReportCursorSelectedEntryReportEntryCursors = Nothing}) <$> genValid) $ \erc ->
        entryReportCursorPrev @(EntryReportCursor ()) erc `shouldBe` Nothing
  describe "entryReportCursorFirst" $ it "produces valid cursors" $ producesValidsOnValids @(EntryReportCursor ()) entryReportCursorFirst
  describe "entryReportCursorLast" $ it "produces valid cursors" $ producesValidsOnValids @(EntryReportCursor ()) entryReportCursorLast
  describe "makeEntryEntryCursor" $ it "produces valid cursors" $ producesValidsOnValids3 (makeEntryReportEntryCursor @())
