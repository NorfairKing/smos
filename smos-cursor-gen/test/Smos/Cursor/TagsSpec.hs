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
    describe "tagsCursorDelete" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorDelete
    describe "tagsCursorRemove" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorRemove
    describe "tagsCursorSelectPrev" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorSelectPrev
    describe "tagsCursorSelectNext" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorSelectNext
    describe "tagsCursorSelectPrevChar" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorSelectPrevChar
    describe "tagsCursorSelectNextChar" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorSelectNextChar
    describe "tagsCursorSelectPrevTag" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorSelectPrevTag
    describe "tagsCursorSelectNextTag" $
        it "produces valid tags cursors" $
        producesValidsOnValids tagsCursorSelectNextTag
