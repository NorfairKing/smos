{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.PropertiesSpec where

import Test.Hspec
import Test.Validity

import Cursor.Types

import Smos.Data.Gen ()

import Smos.Cursor.Properties
import Smos.Cursor.Properties.Gen ()

spec :: Spec
spec = do
    eqSpec @PropertiesCursor
    genValidSpec @PropertiesCursor
    describe "emptyPropertiesCursor" $
        it "is a valid cursor" $ shouldBeValid emptyPropertiesCursor
    describe "makePropertiesCursor" $
        it "produces valid cursors" $
        producesValidsOnValids makePropertiesCursor
    describe "rebuildPropertiesCursor" $ do
        it "produces valid cursors" $
            producesValidsOnValids rebuildPropertiesCursor
        it "is the inverse of makePropertiesCursor" $
            inverseFunctionsOnValid makePropertiesCursor rebuildPropertiesCursor
    describe "propertiesCursorToggleSelected" $ do
        it "produces valid cursors" $
            producesValidsOnValids propertiesCursorToggleSelected
        it "is a movement" pending
    describe "propertiesCursorSelectPrevChar" $ do
        it "produces valid cursors" $
            producesValidsOnValids propertiesCursorSelectPrevChar
        it "is a movement" pending
    describe "propertiesCursorSelectNextChar" $ do
        it "produces valid cursors" $
            producesValidsOnValids propertiesCursorSelectNextChar
        it "is a movement" pending
    describe "propertiesCursorSelectPrevProperty" $ do
        it "produces valid cursors" $
            producesValidsOnValids propertiesCursorSelectPrevProperty
        it "is a movement" pending
    describe "propertiesCursorSelectNextProperty" $ do
        it "produces valid cursors" $
            producesValidsOnValids propertiesCursorSelectNextProperty
        it "is a movement" pending
    describe "propertiesCursorInsert" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 propertiesCursorInsert
    describe "propertiesCursorAppend" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 propertiesCursorAppend
    describe "propertiesCursorRemove" $ do
        it "produces valid cursors" $
            producesValidsOnValids propertiesCursorRemove
    describe "propertiesCursorDelete" $ do
        it "produces valid cursors" $
            producesValidsOnValids propertiesCursorDelete
        it "is the inverse of propertiesCursorStartNewPropertyBefore" $
            forAllValid $ \pc ->
                case propertiesCursorDelete
                         (propertiesCursorStartNewPropertyBefore pc) of
                    Nothing ->
                        expectationFailure "Deletion should have been possible."
                    Just Deleted ->
                        expectationFailure
                            "Should not have been deleted entirely."
                    Just (Updated pc') ->
                        rebuildPropertiesCursor pc' `shouldBe`
                        rebuildPropertiesCursor pc
    describe "propertiesCursorRemoveProperty" $ do
        it "produces valid cursors" $
            producesValidsOnValids propertiesCursorRemoveProperty
        it "is the inverse of propertiesCursorStartNewPropertyBefore" $
            forAllValid $ \pc ->
                case propertiesCursorRemove
                         (propertiesCursorStartNewPropertyBefore pc) of
                    Nothing ->
                        expectationFailure "Removal should have been possible."
                    Just Deleted ->
                        expectationFailure
                            "Should not have been deleted entirely."
                    Just (Updated pc') ->
                        rebuildPropertiesCursor pc' `shouldBe`
                        rebuildPropertiesCursor pc
    describe "propertiesCursorDeleteProperty" $
        it "produces valid cursors" $
            producesValidsOnValids propertiesCursorDeleteProperty
    describe "propertiesCursorRemovePropertyAndSelectPrevious" $
        it "produces valid cursors" $
        producesValidsOnValids propertiesCursorRemovePropertyAndSelectPrevious
    describe "propertiesCursorDeletePropertyAndSelectNext" $
        it "produces valid cursors" $
        producesValidsOnValids propertiesCursorDeletePropertyAndSelectNext
    describe "propertiesCursorStartNewPropertyBefore" $
        it "produces valid cursors" $
        producesValidsOnValids propertiesCursorStartNewPropertyBefore
    describe "propertiesCursorStartNewPropertyAfter" $
        it "produces valid cursors" $
        producesValidsOnValids propertiesCursorStartNewPropertyAfter
    describe "propertiesCursorAddOrSelect" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 propertiesCursorAddOrSelect
    describe "propertiesCursorSet" $ do
        it "produces valid cursors" $
            producesValidsOnValids3 propertiesCursorSet
