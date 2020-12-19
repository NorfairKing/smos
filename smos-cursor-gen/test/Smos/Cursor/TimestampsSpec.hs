{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.TimestampsSpec where

import Cursor.FuzzyLocalTime.Gen ()
import Smos.Cursor.Timestamps
import Smos.Cursor.Timestamps.Gen ()
import Smos.Data.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @TimestampsCursor
  describe "makeTimestampsCursor" $
    it "produces valid cursors" $
      producesValidsOnValids makeTimestampsCursor
  describe "startTimestampsCursor" $
    it "produces valid cursors" $
      producesValidsOnValids2 startTimestampsCursor
  describe "rebuildTimestampsCursor" $ do
    it "produces valid cursors" $ producesValidsOnValids rebuildTimestampsCursor
    it "is the inverse of makeTimestampsCursor" $
      inverseFunctionsOnValid makeTimestampsCursor rebuildTimestampsCursor
  describe "timestampsCursorToggleSelected" $
    it "produces valid cursors" $
      producesValidsOnValids timestampsCursorToggleSelected
  describe "timestampsCursorInsertChar" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ timestampsCursorInsertChar '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ timestampsCursorInsertChar '\55810' tsc
    it "produces valid cursors" $ producesValidsOnValids2 timestampsCursorInsertChar
  describe "timestampsCursorAppendChar" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ timestampsCursorAppendChar '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ timestampsCursorAppendChar '\55810' tsc
    it "produces valid cursors" $ producesValidsOnValids2 timestampsCursorAppendChar
  describe "timestampsCursorRemoveChar" $
    it "produces valid cursors" $
      producesValidsOnValids timestampsCursorRemoveChar
  describe "timestampsCursorDeleteChar" $
    it "produces valid cursors" $
      producesValidsOnValids timestampsCursorDeleteChar
  describe "timestampsCursorSelectNextChar" $
    it "produces valid cursors" $
      producesValidsOnValids timestampsCursorSelectNextChar
  describe "timestampsCursorSelectPrevChar" $
    it "produces valid cursors" $
      producesValidsOnValids timestampsCursorSelectPrevChar
  describe "timestampsCursorInsertEmptyAndSelect" $
    it "produces valid cursors" $
      producesValidsOnValids2 timestampsCursorInsertEmptyAndSelect
  describe "timestampsCursorInsertAndSelect" $
    it "produces valid cursors" $
      producesValidsOnValids3 timestampsCursorInsertAndSelect
  describe "timestampsCursorAppendEmptyAndSelect" $
    it "produces valid cursors" $
      producesValidsOnValids2 timestampsCursorAppendEmptyAndSelect
  describe "timestampsCursorAppendAndSelect" $
    it "produces valid cursors" $
      producesValidsOnValids3 timestampsCursorAppendAndSelect
  describe "timestampsCursorSelectOrAdd" $
    it "produces valid cursors" $
      producesValidsOnValids3 timestampsCursorSelectOrAdd
