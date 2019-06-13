{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.EntrySpec where

import Test.Hspec
import Test.Validity
import Test.Validity.Optics

import Smos.Data.Gen ()

import Smos.Cursor.Entry
import Smos.Cursor.Entry.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @EntryCursor
  genValidSpec @EntryCursor
  eqSpecOnValid @EntryCursorSelection
  genValidSpec @EntryCursorSelection
  describe "makeEntryCursor" $
    it "produces valid cursors" $ producesValidsOnValids makeEntryCursor
  describe "emptyEntryCursor" $ it "is valid" $ shouldBeValid emptyEntryCursor
  describe "rebuildEntryCursor" $ do
    it "produces valid entries" $ producesValidsOnValids rebuildEntryCursor
    it "is the inverse of makeEntryCursor" $
      inverseFunctionsOnValid makeEntryCursor rebuildEntryCursor
  describe "entryCursorHeaderCursorL" $ lensSpecOnValid entryCursorHeaderCursorL
  describe "entryCursorContentsCursorL" $
    lensSpecOnValid entryCursorContentsCursorL
  describe "entryCursorStateHistoryCursorL" $
    lensSpecOnValid entryCursorStateHistoryCursorL
  describe "entryCursorTagsCursorL" $ lensSpecOnValid entryCursorTagsCursorL
  describe "entryCursorTimestampsCursorL" $
    lensSpecOnValid entryCursorTimestampsCursorL
  describe "entryCursorLogbookCursorL" $
    lensSpecOnValid entryCursorLogbookCursorL
  describe "entryCursorSelectionL" $ lensSpecOnValid entryCursorSelectionL
  describe "entryCursorSelect" $
    it "produces valid cursors" $ producesValidsOnValids2 entryCursorSelect
  describe "entryCursorSelectHeaderAtStart" $
    it "produces valid cursors" $
    producesValidsOnValids entryCursorSelectHeaderAtStart
  describe "entryCursorSelectHeaderAtEnd" $
    it "produces valid cursors" $
    producesValidsOnValids entryCursorSelectHeaderAtEnd
  describe "entryCursorSelectContents" $
    it "produces valid cursors" $
    producesValidsOnValids entryCursorSelectContents
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
    it "produces valid cursors" $ producesValidsOnValids entryCursorSelectTags
  describe "entryCursorSelectLogbook" $
    it "produces valid cursors" $
    producesValidsOnValids entryCursorSelectLogbook
