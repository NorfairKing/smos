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
      producesValid makeTimestampsCursor
  describe "startTimestampsCursor" $
    it "produces valid cursors" $
      producesValid2 startTimestampsCursor
  describe "rebuildTimestampsCursor" $ do
    it "produces valid cursors" $ producesValid rebuildTimestampsCursor
    it "is the inverse of makeTimestampsCursor" $
      inverseFunctions makeTimestampsCursor rebuildTimestampsCursor
  describe "timestampsCursorToggleSelected" $
    it "produces valid cursors" $
      producesValid timestampsCursorToggleSelected
  describe "timestampsCursorInsertChar" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ timestampsCursorInsertChar '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ timestampsCursorInsertChar '\55810' tsc
    it "produces valid cursors" $ producesValid2 timestampsCursorInsertChar
  describe "timestampsCursorAppendChar" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ timestampsCursorAppendChar '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ timestampsCursorAppendChar '\55810' tsc
    it "produces valid cursors" $ producesValid2 timestampsCursorAppendChar
  describe "timestampsCursorRemoveChar" $
    it "produces valid cursors" $
      producesValid timestampsCursorRemoveChar
  describe "timestampsCursorDeleteChar" $
    it "produces valid cursors" $
      producesValid timestampsCursorDeleteChar
  describe "timestampsCursorSelectNextChar" $
    it "produces valid cursors" $
      producesValid timestampsCursorSelectNextChar
  describe "timestampsCursorSelectPrevChar" $
    it "produces valid cursors" $
      producesValid timestampsCursorSelectPrevChar
  describe "timestampsCursorInsertEmptyAndSelect" $
    it "produces valid cursors" $
      producesValid2 timestampsCursorInsertEmptyAndSelect
  describe "timestampsCursorInsertAndSelect" $
    it "produces valid cursors" $
      producesValid3 timestampsCursorInsertAndSelect
  describe "timestampsCursorAppendEmptyAndSelect" $
    it "produces valid cursors" $
      producesValid2 timestampsCursorAppendEmptyAndSelect
  describe "timestampsCursorAppendAndSelect" $
    it "produces valid cursors" $
      producesValid3 timestampsCursorAppendAndSelect
  describe "timestampsCursorSelectOrAdd" $
    it "produces valid cursors" $
      producesValid3 timestampsCursorSelectOrAdd
