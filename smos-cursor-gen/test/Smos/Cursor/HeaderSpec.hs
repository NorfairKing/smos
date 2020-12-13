{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.HeaderSpec where

import Smos.Cursor.Header
import Smos.Cursor.Header.Gen ()
import Smos.Data.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @HeaderCursor
  shrinkValidSpecWithLimit @HeaderCursor 100
  describe "makeHeaderCursor" $
    it "produces valid cursors" $
      producesValidsOnValids makeHeaderCursor
  describe "rebuildHeaderCursor" $ do
    it "produces valid cursors" $ producesValidsOnValids rebuildHeaderCursor
    it "is the inverse of makeHeaderCursor" $
      inverseFunctionsOnValid makeHeaderCursor rebuildHeaderCursor
  describe "headerCursorInsert" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ headerCursorInsert '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ headerCursorInsert '\55810' tsc
    it "produces valid cursors" $ producesValidsOnValids2 headerCursorInsert
  describe "headerCursorAppend" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ headerCursorAppend '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ headerCursorAppend '\55810' tsc
    it "produces valid cursors" $ producesValidsOnValids2 headerCursorAppend
  describe "headerCursorInsertString" $ do
    it "produces valid cursorsString" $ producesValidsOnValids2 headerCursorInsertString
  describe "headerCursorAppend" $ do
    it "produces valid cursorsString" $ producesValidsOnValids2 headerCursorAppendString
  describe "headerCursorRemove" $
    it "produces valid cursors" $
      producesValidsOnValids headerCursorRemove
  describe "headerCursorDelete" $
    it "produces valid cursors" $
      producesValidsOnValids headerCursorDelete
  describe "headerCursorSelectPrev" $
    it "produces valid cursors" $
      producesValidsOnValids headerCursorSelectPrev
  describe "headerCursorSelectNext" $
    it "produces valid cursors" $
      producesValidsOnValids headerCursorSelectNext
  describe "headerCursorSelectStart" $
    it "produces valid cursors" $
      producesValidsOnValids headerCursorSelectStart
  describe "headerCursorSelectEnd" $
    it "produces valid cursors" $
      producesValidsOnValids headerCursorSelectEnd
  describe "headerCursorSelectPrevWord" $
    it "produces valid cursors" $
      producesValidsOnValids headerCursorSelectPrevWord
  describe "headerCursorSelectNextWord" $
    it "produces valid cursors" $
      producesValidsOnValids headerCursorSelectNextWord
  describe "headerCursorSelectBeginWord" $
    it "produces valid cursors" $
      producesValidsOnValids headerCursorSelectBeginWord
  describe "headerCursorSelectEndWord" $
    it "produces valid cursors" $
      producesValidsOnValids headerCursorSelectEndWord
