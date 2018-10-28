{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.TagSpec where

import Test.Hspec
import Test.Validity

import Smos.Data.Gen ()

import Smos.Cursor.Tag
import Smos.Cursor.Tag.Gen ()

spec :: Spec
spec = do
    eqSpec @TagCursor
    genValidSpec @TagCursor
    describe "makeTagCursor" $
        it "produces valid cursors" $ producesValidsOnValids makeTagCursor
    describe "rebuildTagCursor" $ do
        it "produces valid cursors" $ producesValidsOnValids rebuildTagCursor
        it "is the inverse of makeTagCursor" $
            inverseFunctionsOnValid makeTagCursor rebuildTagCursor
    describe "tagCursorInsert" $
        it "produces valid tag cursors" $
        producesValidsOnValids2 tagCursorInsert
    describe "tagCursorAppend" $
        it "produces valid tag cursors" $
        producesValidsOnValids2 tagCursorAppend
    describe "tagCursorDelete" $
        it "produces valid tag cursors" $
        producesValidsOnValids tagCursorDelete
    describe "tagCursorRemove" $
        it "produces valid tag cursors" $
        producesValidsOnValids tagCursorRemove
