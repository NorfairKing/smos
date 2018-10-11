{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.EditorSpec where

import Test.Hspec
import Test.Validity
import Test.Validity.Optics

import Smos.Data.Gen ()

import Smos.Types
import Smos.Cursor.Editor.Gen ()

spec :: Spec
spec = do
    eqSpec @EditorCursor
    genValidSpec @EditorCursor
    describe "makeEditorCursor" $
        it "produces valid cursors" $ producesValidsOnValids makeEditorCursor
    describe "rebuildEditorCursor" $ do
        it "produces valid cursors" $ producesValidsOnValids rebuildEditorCursor
        it "is the inverse of makeFileCursor" $
            inverseFunctionsOnValid makeEditorCursor rebuildEditorCursor
    describe "editorCursorSmosFileCursorL" $
        lensSpecOnValid editorCursorSmosFileCursorL
    describe "editorCursorSelectionL" $ lensSpecOnValid editorCursorSelectionL
    describe "editorCursorDebugL" $ lensSpecOnValid editorCursorDebugL
    describe "editorCursorShowDebug" $
        it "produces valid cursors" $
        producesValidsOnValids editorCursorShowDebug
    describe "editorCursorHideDebug" $
        it "produces valid cursors" $
        producesValidsOnValids editorCursorHideDebug
    describe "editorCursorToggleDebug" $
        it "produces valid cursors" $
        producesValidsOnValids editorCursorToggleDebug
