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
      producesValid makeHeaderCursor
  describe "rebuildHeaderCursor" $ do
    it "produces valid cursors" $ producesValid rebuildHeaderCursor
    it "is the inverse of makeHeaderCursor" $
      inverseFunctions makeHeaderCursor rebuildHeaderCursor
  describe "headerCursorInsert" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ headerCursorInsert '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ headerCursorInsert '\55810' tsc
    it "produces valid cursors" $ producesValid2 headerCursorInsert
  describe "headerCursorAppend" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ headerCursorAppend '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ headerCursorAppend '\55810' tsc
    it "produces valid cursors" $ producesValid2 headerCursorAppend
  describe "headerCursorInsertString" $ do
    it "produces valid cursorsString" $ producesValid2 headerCursorInsertString
  describe "headerCursorAppend" $ do
    it "produces valid cursorsString" $ producesValid2 headerCursorAppendString
  describe "headerCursorRemove" $
    it "produces valid cursors" $
      producesValid headerCursorRemove
  describe "headerCursorDelete" $
    it "produces valid cursors" $
      producesValid headerCursorDelete
  describe "headerCursorSelectPrev" $
    it "produces valid cursors" $
      producesValid headerCursorSelectPrev
  describe "headerCursorSelectNext" $
    it "produces valid cursors" $
      producesValid headerCursorSelectNext
  describe "headerCursorSelectStart" $
    it "produces valid cursors" $
      producesValid headerCursorSelectStart
  describe "headerCursorSelectEnd" $
    it "produces valid cursors" $
      producesValid headerCursorSelectEnd
  describe "headerCursorSelectPrevWord" $
    it "produces valid cursors" $
      producesValid headerCursorSelectPrevWord
  describe "headerCursorSelectNextWord" $
    it "produces valid cursors" $
      producesValid headerCursorSelectNextWord
  describe "headerCursorSelectBeginWord" $
    it "produces valid cursors" $
      producesValid headerCursorSelectBeginWord
  describe "headerCursorSelectEndWord" $
    it "produces valid cursors" $
      producesValid headerCursorSelectEndWord
