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
  describe "entryReportCursorEntryEntryCursorsL" $ lensSpec (entryReportCursorEntryReportEntryCursorsL @())
  describe "entryReportCursorSelectedEntryEntryCursorsL" $ lensSpec (entryReportCursorSelectedEntryReportEntryCursorsL @())
  describe "entryReportCursorSelectionL" $ lensSpec (entryReportCursorSelectionL @())
  describe "entryReportCursorFilterBarL" $ lensSpec (entryReportCursorFilterBarL @())
  describe "entryReportCursorNext" $ do
    it "produces valid cursors" $
      producesValid @(EntryReportCursor ())
        entryReportCursorNext
    it "fails when there are no selected entries" $
      forAll ((\erc -> erc {entryReportCursorSelectedEntryReportEntryCursors = Nothing}) <$> genValid) $ \erc ->
        entryReportCursorNext @(EntryReportCursor ()) erc `shouldBe` Nothing
  describe "entryReportCursorPrev" $ do
    it "produces valid cursors" $ producesValid @(EntryReportCursor ()) entryReportCursorPrev
    it "fails when there are no selected entries" $
      forAll ((\erc -> erc {entryReportCursorSelectedEntryReportEntryCursors = Nothing}) <$> genValid) $ \erc ->
        entryReportCursorPrev @(EntryReportCursor ()) erc `shouldBe` Nothing
  describe "entryReportCursorFirst" $ it "produces valid cursors" $ producesValid @(EntryReportCursor ()) entryReportCursorFirst
  describe "entryReportCursorLast" $ it "produces valid cursors" $ producesValid @(EntryReportCursor ()) entryReportCursorLast
  describe "makeEntryEntryCursor" $ it "produces valid cursors" $ producesValid3 (makeEntryReportEntryCursor @())
