{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.ContentsSpec where

import Control.Monad
import Cursor.Types
import Smos.Cursor.Contents
import Smos.Cursor.Contents.Gen ()
import Smos.Data.Gen ()
import Test.Syd
import Test.Syd.Validity
import Text.Show.Pretty (ppShow)

spec :: Spec
spec = do
  genValidSpec @ContentsCursor
  describe "makeContentsCursor" $
    it "produces valid cursors" $
      producesValidsOnValids makeContentsCursor
  describe "rebuildContentsCursor" $ do
    it "produces valid cursors" $ producesValidsOnValids rebuildContentsCursor
    it "is the inverse of makeContentsCursor" $
      inverseFunctionsOnValid makeContentsCursor rebuildContentsCursor
  describe "makeContentsCursorWithSelection" $ do
    it "produces valid cursors" $ producesValidsOnValids3 makeContentsCursorWithSelection
    it "rebuilds the current cursor when given the current selection" $
      forAllValid $
        \cc ->
          let (x, y) = contentsCursorSelection cc
              c = rebuildContentsCursor cc
           in case makeContentsCursorWithSelection x y c of
                Nothing ->
                  expectationFailure "makeContentsCursorWithSelection should not have failed."
                Just cc' ->
                  unless (cc' == cc) $
                    expectationFailure $
                      unlines
                        [ "expected",
                          ppShow cc,
                          "actual",
                          ppShow cc',
                          "The selection of the original (expected) cursor was:",
                          show (x, y),
                          "The rebuilt contents were:",
                          ppShow c
                        ]
  describe "contentsCursorSelection" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorSelection
  describe "contentsCursorSelectPrevLine" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorSelectPrevLine
  describe "contentsCursorSelectPrevLineOrTheStartOfThisLine" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorSelectPrevLineOrTheStartOfThisLine
  describe "contentsCursorSelectNextLine" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorSelectNextLine
  describe "contentsCursorSelectNextLineOrTheEndOfThisLine" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorSelectNextLineOrTheEndOfThisLine
  describe "contentsCursorSelectFirstLine" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorSelectFirstLine
  describe "contentsCursorSelectLastLine" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorSelectLastLine
  describe "contentsCursorSelectPrevChar" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorSelectPrevChar
  describe "contentsCursorSelectNextChar" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorSelectNextChar
  describe "contentsCursorSelectPrevWord" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorSelectPrevWord
  describe "contentsCursorSelectNextWord" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorSelectNextWord
  describe "contentsCursorSelectBeginWord" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorSelectBeginWord
  describe "contentsCursorSelectEndWord" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorSelectEndWord
  describe "contentsCursorIndexOnLine" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorIndexOnLine
  describe "contentsCursorSelectIndexOnLine" $
    it "produces valid cursors" $
      producesValidsOnValids2 contentsCursorSelectIndexOnLine
  describe "contentsCursorInsertChar" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ contentsCursorInsertChar '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ contentsCursorInsertChar '\55810' tsc
    it "produces valid cursors" $ producesValidsOnValids2 contentsCursorInsertChar
  describe "contentsCursorAppendChar" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ contentsCursorAppendChar '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ contentsCursorAppendChar '\55810' tsc
    it "produces valid cursors" $ producesValidsOnValids2 contentsCursorAppendChar
  describe "contentsCursorInsertNewline" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorInsertNewline
  describe "contentsCursorAppendNewline" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorAppendNewline
  describe "contentsCursorRemove" $ do
    it "produces valid cursors" $ producesValidsOnValids contentsCursorRemove
    it "removes the contents cursor if the contents cursor was empty" $
      contentsCursorRemove emptyContentsCursor `shouldBe` Just Deleted
  describe "contentsCursorDelete" $ do
    it "produces valid cursors" $ producesValidsOnValids contentsCursorDelete
    it "removes the contents cursor if the contents cursor was empty" $
      contentsCursorDelete emptyContentsCursor `shouldBe` Just Deleted
  describe "contentsCursorSelectStartOfLine" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorSelectStartOfLine
  describe "contentsCursorSelectEndOfLine" $
    it "produces valid cursors" $
      producesValidsOnValids contentsCursorSelectEndOfLine
