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
  describe "makeEntryCursor" $ it "produces valid cursors" $ producesValidsOnValids makeEntryCursor
  describe "emptyEntryCursor" $ it "is valid" $ shouldBeValid emptyEntryCursor
  describe "rebuildEntryCursor" $ do
    it "produces valid entries" $ producesValidsOnValids rebuildEntryCursor
    it "is the inverse of makeEntryCursor" $
      inverseFunctionsOnValid makeEntryCursor rebuildEntryCursor
  describe "entryCursorHeaderCursorL" $ lensSpecOnValid entryCursorHeaderCursorL
  describe "entryCursorContentsCursorL" $ lensSpecOnValid entryCursorContentsCursorL
  describe "entryCursorStateHistoryCursorL" $ lensSpecOnValid entryCursorStateHistoryCursorL
  describe "entryCursorTagsCursorL" $ lensSpecOnValid entryCursorTagsCursorL
  describe "entryCursorTimestampsCursorL" $ lensSpecOnValid entryCursorTimestampsCursorL
  describe "entryCursorLogbookCursorL" $ lensSpecOnValid entryCursorLogbookCursorL
  describe "entryCursorSelectionL" $ lensSpecOnValid entryCursorSelectionL
  describe "entryCursorSelect" $
    it "produces valid cursors" $
      producesValidsOnValids2 entryCursorSelect
  describe "entryCursorSelectHeaderAtStart" $
    it "produces valid cursors" $
      producesValidsOnValids entryCursorSelectHeaderAtStart
  describe "entryCursorSelectHeaderAtEnd" $
    it "produces valid cursors" $
      producesValidsOnValids entryCursorSelectHeaderAtEnd
  describe "entryCursorSelectContentsAtStart" $
    it "produces valid cursors" $
      producesValidsOnValids entryCursorSelectContentsAtStart
  describe "entryCursorSelectContentsAtEnd" $
    it "produces valid cursors" $
      producesValidsOnValids entryCursorSelectContentsAtEnd
  describe "entryCursorSelectTimestamps" $
    it "produces valid cursors" $
      producesValidsOnValids entryCursorSelectTimestamps
  describe "entryCursorSelectProperties" $
    it "produces valid cursors" $
      producesValidsOnValids entryCursorSelectProperties
  describe "entryCursorSelectStateHistory" $
    it "produces valid cursors" $
      producesValidsOnValids entryCursorSelectStateHistory
  describe "entryCursorSelectTags" $
    it "produces valid cursors" $
      producesValidsOnValids entryCursorSelectTags
  describe "entryCursorSelectLogbook" $
    it "produces valid cursors" $
      producesValidsOnValids entryCursorSelectLogbook
