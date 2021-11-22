{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.ContentsSpec where

import Cursor.Types
import Smos.Cursor.Contents
import Smos.Cursor.Contents.Gen ()
import Smos.Data.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @ContentsCursor
  describe "makeContentsCursor" $
    it "produces valid cursors" $
      producesValid makeContentsCursor
  describe "rebuildContentsCursor" $ do
    it "produces valid cursors" $ producesValid rebuildContentsCursor
    it "is the inverse of makeContentsCursor" $
      inverseFunctions makeContentsCursor rebuildContentsCursor
  describe "makeContentsCursorWithSelection" $ do
    it "produces valid cursors" $ producesValid3 makeContentsCursorWithSelection
    it "rebuilds the current cursor when given the current selection" $
      forAllValid $
        \cc ->
          let (x, y) = contentsCursorSelection cc
              c = rebuildContentsCursor cc
           in case makeContentsCursorWithSelection x y c of
                Nothing ->
                  expectationFailure "makeContentsCursorWithSelection should not have failed."
                Just cc' ->
                  let ctx =
                        unlines
                          [ "The selection of the original (expected) cursor was:",
                            show (x, y),
                            "The rebuilt contents were:",
                            ppShow c
                          ]
                   in context ctx $ cc' `shouldBe` cc
  describe "contentsCursorSelection" $
    it "produces valid cursors" $
      producesValid contentsCursorSelection
  describe "contentsCursorSelectPrevLine" $
    it "produces valid cursors" $
      producesValid contentsCursorSelectPrevLine
  describe "contentsCursorSelectPrevLineOrTheStartOfThisLine" $
    it "produces valid cursors" $
      producesValid contentsCursorSelectPrevLineOrTheStartOfThisLine
  describe "contentsCursorSelectNextLine" $
    it "produces valid cursors" $
      producesValid contentsCursorSelectNextLine
  describe "contentsCursorSelectNextLineOrTheEndOfThisLine" $
    it "produces valid cursors" $
      producesValid contentsCursorSelectNextLineOrTheEndOfThisLine
  describe "contentsCursorSelectFirstLine" $
    it "produces valid cursors" $
      producesValid contentsCursorSelectFirstLine
  describe "contentsCursorSelectLastLine" $
    it "produces valid cursors" $
      producesValid contentsCursorSelectLastLine
  describe "contentsCursorSelectPrevChar" $
    it "produces valid cursors" $
      producesValid contentsCursorSelectPrevChar
  describe "contentsCursorSelectNextChar" $
    it "produces valid cursors" $
      producesValid contentsCursorSelectNextChar
  describe "contentsCursorSelectPrevWord" $
    it "produces valid cursors" $
      producesValid contentsCursorSelectPrevWord
  describe "contentsCursorSelectNextWord" $
    it "produces valid cursors" $
      producesValid contentsCursorSelectNextWord
  describe "contentsCursorSelectBeginWord" $
    it "produces valid cursors" $
      producesValid contentsCursorSelectBeginWord
  describe "contentsCursorSelectEndWord" $
    it "produces valid cursors" $
      producesValid contentsCursorSelectEndWord
  describe "contentsCursorIndexOnLine" $
    it "produces valid cursors" $
      producesValid contentsCursorIndexOnLine
  describe "contentsCursorSelectIndexOnLine" $
    it "produces valid cursors" $
      producesValid2 contentsCursorSelectIndexOnLine
  describe "contentsCursorInsertChar" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ contentsCursorInsertChar '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ contentsCursorInsertChar '\55810' tsc
    it "produces valid cursors" $ producesValid2 contentsCursorInsertChar
  describe "contentsCursorAppendChar" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ contentsCursorAppendChar '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ contentsCursorAppendChar '\55810' tsc
    it "produces valid cursors" $ producesValid2 contentsCursorAppendChar
  describe "contentsCursorInsertNewline" $
    it "produces valid cursors" $
      producesValid contentsCursorInsertNewline
  describe "contentsCursorAppendNewline" $
    it "produces valid cursors" $
      producesValid contentsCursorAppendNewline
  describe "contentsCursorRemove" $ do
    it "produces valid cursors" $ producesValid contentsCursorRemove
    it "removes the contents cursor if the contents cursor was empty" $
      contentsCursorRemove emptyContentsCursor `shouldBe` Just Deleted
  describe "contentsCursorDelete" $ do
    it "produces valid cursors" $ producesValid contentsCursorDelete
    it "removes the contents cursor if the contents cursor was empty" $
      contentsCursorDelete emptyContentsCursor `shouldBe` Just Deleted
  describe "contentsCursorSelectStartOfLine" $
    it "produces valid cursors" $
      producesValid contentsCursorSelectStartOfLine
  describe "contentsCursorSelectEndOfLine" $
    it "produces valid cursors" $
      producesValid contentsCursorSelectEndOfLine
