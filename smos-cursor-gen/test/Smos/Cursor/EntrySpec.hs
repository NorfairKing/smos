{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.EntrySpec where

import Smos.Cursor.Entry
import Smos.Cursor.Entry.Gen ()
import Smos.Data.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Lens

spec :: Spec
spec = do
  genValidSpec @EntryCursor
  genValidSpec @EntryCursorSelection
  describe "makeEntryCursor" $ it "produces valid cursors" $ producesValid makeEntryCursor
  describe "emptyEntryCursor" $ it "is valid" $ shouldBeValid emptyEntryCursor
  describe "rebuildEntryCursor" $ do
    it "produces valid entries" $ producesValid rebuildEntryCursor
    it "is the inverse of makeEntryCursor" $
      inverseFunctions makeEntryCursor rebuildEntryCursor
  describe "entryCursorHeaderCursorL" $ lensSpec entryCursorHeaderCursorL
  describe "entryCursorContentsCursorL" $ lensSpec entryCursorContentsCursorL
  describe "entryCursorStateHistoryCursorL" $ lensSpec entryCursorStateHistoryCursorL
  describe "entryCursorTagsCursorL" $ lensSpec entryCursorTagsCursorL
  describe "entryCursorTimestampsCursorL" $ lensSpec entryCursorTimestampsCursorL
  describe "entryCursorLogbookCursorL" $ lensSpec entryCursorLogbookCursorL
  describe "entryCursorSelectionL" $ lensSpec entryCursorSelectionL
  describe "entryCursorSelect" $
    it "produces valid cursors" $
      producesValid2 entryCursorSelect
  describe "entryCursorSelectHeaderAtStart" $
    it "produces valid cursors" $
      producesValid entryCursorSelectHeaderAtStart
  describe "entryCursorSelectHeaderAtEnd" $
    it "produces valid cursors" $
      producesValid entryCursorSelectHeaderAtEnd
  describe "entryCursorSelectContentsAtStart" $
    it "produces valid cursors" $
      producesValid entryCursorSelectContentsAtStart
  describe "entryCursorSelectContentsAtEnd" $
    it "produces valid cursors" $
      producesValid entryCursorSelectContentsAtEnd
  describe "entryCursorSelectTimestamps" $
    it "produces valid cursors" $
      producesValid entryCursorSelectTimestamps
  describe "entryCursorSelectProperties" $
    it "produces valid cursors" $
      producesValid entryCursorSelectProperties
  describe "entryCursorSelectStateHistory" $
    it "produces valid cursors" $
      producesValid entryCursorSelectStateHistory
  describe "entryCursorSelectTags" $
    it "produces valid cursors" $
      producesValid entryCursorSelectTags
  describe "entryCursorSelectLogbook" $
    it "produces valid cursors" $
      producesValid entryCursorSelectLogbook
