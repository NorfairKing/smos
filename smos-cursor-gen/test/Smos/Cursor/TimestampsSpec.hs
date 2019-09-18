{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.TimestampsSpec where

import Test.Hspec
import Test.Validity
import Test.Validity.Optics

import Cursor.FuzzyLocalTime.Gen ()

import Smos.Data.Gen ()

import Smos.Cursor.Timestamps
import Smos.Cursor.Timestamps.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @TimestampsCursor
  genValidSpec @TimestampsCursor
  describe "makeTimestampsCursor" $
    it "produces valid cursors" $ producesValidsOnValids makeTimestampsCursor
  describe "startTimestampsCursor" $
    it "produces valid cursors" $ producesValidsOnValids2 startTimestampsCursor
  describe "rebuildTimestampsCursor" $ do
    it "produces valid cursors" $ producesValidsOnValids rebuildTimestampsCursor
    it "is the inverse of makeTimestampsCursor" $
      inverseFunctionsOnValid makeTimestampsCursor rebuildTimestampsCursor
  describe "timestampsCursorCurrentTextCursorL" $
    lensSpecOnValid timestampsCursorCurrentTextCursorL
  describe "timestampsCursorToggleSelected" $
    it "produces valid cursors" $
    producesValidsOnValids timestampsCursorToggleSelected
  describe "timestampsCursorInsertChar" $
    it "produces valid cursors" $
    producesValidsOnValids2 timestampsCursorInsertChar
  describe "timestampsCursorAppendChar" $
    it "produces valid cursors" $
    producesValidsOnValids2 timestampsCursorAppendChar
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
