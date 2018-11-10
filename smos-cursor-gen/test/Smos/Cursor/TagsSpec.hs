{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.TagsSpec where

import Test.Hspec
import Test.Validity

import Smos.Data.Gen ()

import Smos.Cursor.Tags
import Smos.Cursor.Tags.Gen ()

spec :: Spec
spec = do
    eqSpec @TagsCursor
    genValidSpec @TagsCursor
    describe "makeTagsCursor" $
        it "produces valid cursors" $ producesValidsOnValids makeTagsCursor
    describe "singletonTagsCursor" $
        it "produces valid cursors" $ producesValidsOnValids singletonTagsCursor
    describe "rebuildTagsCursor" $ do
        it "produces valid cursors" $ producesValidsOnValids rebuildTagsCursor
        it "is the inverse of makeTagsCursor" $
            inverseFunctionsOnValid makeTagsCursor rebuildTagsCursor
    describe "tagsCursorSetTag" $
        it "produces valid cursors" $ producesValidsOnValids2 tagsCursorSetTag
    describe "tagsCursorUnsetTag" $
        it "produces valid cursors" $ producesValidsOnValids2 tagsCursorUnsetTag
    describe "tagsCursorToggleTag" $
        it "produces valid cursors" $
        producesValidsOnValids2 tagsCursorToggleTag
    describe "tagsCursorInsert" $
        it "produces valid tags cursors" $
        producesValidsOnValids2 tagsCursorInsert
    describe "tagsCursorAppend" $
        it "produces valid tags cursors" $
        producesValidsOnValids2 tagsCursorAppend
    describe "tagsCursorInsertTag" $
        it "produces valid tags cursors" $
        producesValidsOnValids2 tagsCursorInsertTag
    describe "tagsCursorAppendTag" $
        it "produces valid tags cursors" $
        producesValidsOnValids2 tagsCursorAppendTag
    describe "tagsCursorInsertAndSelectTag" $
        it "produces valid tags cursors" $
        producesValidsOnValids2 tagsCursorInsertAndSelectTag
    describe "tagsCursorAppendAndSelectTag" $
        it "produces valid tags cursors" $
        producesValidsOnValids2 tagsCursorAppendAndSelectTag
    describe "tagsCursorDelete" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorDelete
    describe "tagsCursorRemove" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorRemove
    describe "tagsCursorSelectPrev" $ do
        it "produces valid tags cursors" $
            producesValidsOnValids tagsCursorSelectPrev
        it "is the inverse of tagsCursorSelectNext" $
            inverseFunctionsIfSucceedOnValid
                tagsCursorSelectNext
                tagsCursorSelectPrev
    describe "tagsCursorSelectNext" $ do
        it "produces valid tags cursors" $
            producesValidsOnValids tagsCursorSelectNext
        it "is the inverse of tagsCursorSelectPrev" $
            inverseFunctionsIfSucceedOnValid
                tagsCursorSelectPrev
                tagsCursorSelectNext
    describe "tagsCursorSelectOrCreatePrev" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorSelectOrCreatePrev
    describe "tagsCursorSelectOrCreateNext" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorSelectOrCreateNext
    describe "tagsCursorSelectPrevChar" $ do
        it "produces valid tags cursors" $
            producesValidsOnValids tagsCursorSelectPrevChar
        it "is the inverse of tagsCursorSelectNextChar" $
            inverseFunctionsIfSucceedOnValid
                tagsCursorSelectNextChar
                tagsCursorSelectPrevChar
    describe "tagsCursorSelectNextChar" $ do
        it "produces valid tags cursors" $
            producesValidsOnValids tagsCursorSelectNextChar
        it "is the inverse of tagsCursorSelectPrevChar" $
            inverseFunctionsIfSucceedOnValid
                tagsCursorSelectPrevChar
                tagsCursorSelectNextChar
    describe "tagsCursorSelectPrevTag" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorSelectPrevTag
    describe "tagsCursorSelectNextTag" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorSelectNextTag
    describe "tagsCursorSelectOrCreatePrevTag" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorSelectOrCreatePrevTag
    describe "tagsCursorSelectOrCreateNextTag" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorSelectOrCreateNextTag
