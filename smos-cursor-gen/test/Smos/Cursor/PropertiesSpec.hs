{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.PropertiesSpec where

import Cursor.Types
import Smos.Cursor.Properties
import Smos.Cursor.Properties.Gen ()
import Smos.Data.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @PropertiesCursor
  describe "emptyPropertiesCursor" $ it "is a valid cursor" $ shouldBeValid emptyPropertiesCursor
  describe "makePropertiesCursor" $
    it "produces valid cursors" $
      producesValid makePropertiesCursor
  describe "rebuildPropertiesCursor" $ do
    it "produces valid cursors" $ producesValid rebuildPropertiesCursor
    it "is the inverse of makePropertiesCursor" $
      inverseFunctions makePropertiesCursor rebuildPropertiesCursor
  describe "propertiesCursorToggleSelected" $ do
    it "produces valid cursors" $ producesValid propertiesCursorToggleSelected
    pending "is a movement"
  describe "propertiesCursorSelectPrevChar" $ do
    it "produces valid cursors" $ producesValid propertiesCursorSelectPrevChar
    pending "is a movement"
  describe "propertiesCursorSelectNextChar" $ do
    it "produces valid cursors" $ producesValid propertiesCursorSelectNextChar
    pending "is a movement"
  describe "propertiesCursorSelectPrevProperty" $ do
    it "produces valid cursors" $ producesValid propertiesCursorSelectPrevProperty
    pending "is a movement"
  describe "propertiesCursorSelectNextProperty" $ do
    it "produces valid cursors" $ producesValid propertiesCursorSelectNextProperty
    pending "is a movement"
  describe "propertiesCursorInsert" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ propertiesCursorInsert '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ propertiesCursorInsert '\55810' tsc
    it "produces valid cursors" $ producesValid2 propertiesCursorInsert
  describe "propertiesCursorAppend" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ propertiesCursorAppend '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ propertiesCursorAppend '\55810' tsc
    it "produces valid cursors" $ producesValid2 propertiesCursorAppend
  describe "propertiesCursorRemove" $
    it "produces valid cursors" $
      producesValid propertiesCursorRemove
  describe "propertiesCursorDelete" $ do
    it "produces valid cursors" $ producesValid propertiesCursorDelete
    it "is the inverse of propertiesCursorStartNewPropertyBefore" $
      forAllValid $
        \pc ->
          case propertiesCursorDelete (propertiesCursorStartNewPropertyBefore pc) of
            Nothing -> expectationFailure "Deletion should have been possible."
            Just Deleted -> expectationFailure "Should not have been deleted entirely."
            Just (Updated pc') -> rebuildPropertiesCursor pc' `shouldBe` rebuildPropertiesCursor pc
  describe "propertiesCursorRemoveProperty" $ do
    it "produces valid cursors" $ producesValid propertiesCursorRemoveProperty
    it "is the inverse of propertiesCursorStartNewPropertyBefore" $
      forAllValid $
        \pc ->
          case propertiesCursorRemove (propertiesCursorStartNewPropertyBefore pc) of
            Nothing -> expectationFailure "Removal should have been possible."
            Just Deleted -> expectationFailure "Should not have been deleted entirely."
            Just (Updated pc') -> rebuildPropertiesCursor pc' `shouldBe` rebuildPropertiesCursor pc
  describe "propertiesCursorDeleteProperty" $
    it "produces valid cursors" $
      producesValid propertiesCursorDeleteProperty
  describe "propertiesCursorRemovePropertyAndSelectPrevious" $
    it "produces valid cursors" $
      producesValid propertiesCursorRemovePropertyAndSelectPrevious
  describe "propertiesCursorDeletePropertyAndSelectNext" $
    it "produces valid cursors" $
      producesValid propertiesCursorDeletePropertyAndSelectNext
  describe "propertiesCursorStartNewPropertyBefore" $
    it "produces valid cursors" $
      producesValid propertiesCursorStartNewPropertyBefore
  describe "propertiesCursorStartNewPropertyAfter" $
    it "produces valid cursors" $
      producesValid propertiesCursorStartNewPropertyAfter
  describe "propertiesCursorAddOrSelect" $
    it "produces valid cursors" $
      producesValid2 propertiesCursorAddOrSelect
  describe "propertiesCursorSet" $
    it "produces valid cursors" $
      producesValid3 propertiesCursorSet
