{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.TagSpec where

import Smos.Cursor.Tag
import Smos.Cursor.Tag.Gen ()
import Smos.Data.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @TagCursor
  describe "makeTagCursor" $ it "produces valid cursors" $ producesValid makeTagCursor
  describe "rebuildTagCursor" $ do
    it "produces valid cursors" $ producesValid rebuildTagCursor
    it "is the inverse of makeTagCursor" $ inverseFunctions makeTagCursor rebuildTagCursor
  describe "tagCursorInsert" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ tagCursorInsert '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ tagCursorInsert '\55810' tsc
    it "produces valid tag cursors" $ producesValid2 tagCursorInsert
  describe "tagCursorAppend" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ tagCursorAppend '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ tagCursorAppend '\55810' tsc
    it "produces valid tag cursors" $ producesValid2 tagCursorAppend
  describe "tagCursorDelete" $
    it "produces valid tag cursors" $
      producesValid tagCursorDelete
  describe "tagCursorRemove" $
    it "produces valid tag cursors" $
      producesValid tagCursorRemove
  describe "tagCursorSelectStart" $
    it "produces valid tag cursors" $
      producesValid tagCursorSelectStart
  describe "tagCursorSelectEnd" $
    it "produces valid tag cursors" $
      producesValid tagCursorSelectEnd
  describe "tagCursorSelectPrevChar" $
    it "produces valid tag cursors" $
      producesValid tagCursorSelectPrevChar
  describe "tagCursorSelectNextChar" $
    it "produces valid tag cursors" $
      producesValid tagCursorSelectNextChar
