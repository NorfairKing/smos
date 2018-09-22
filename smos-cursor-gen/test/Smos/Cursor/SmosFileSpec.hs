{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.SmosFileSpec where

import Test.Hspec
import Test.Validity
import Test.Validity.Optics

import Smos.Data.Gen ()

import Smos.Cursor.SmosFile
import Smos.Cursor.SmosFile.Gen ()

spec :: Spec
spec = do
    eqSpec @SmosFileCursor
    genValidSpec @SmosFileCursor
    describe "makeSmosFileCursor" $
        it "produces valid cursors" $ producesValidsOnValids makeSmosFileCursor
    describe "rebuildSmosFileCursor" $ do
        it "produces valid cursors" $
            producesValidsOnValids rebuildSmosFileCursor
        it "is the inverse of makeFileCursor" $
            inverseFunctionsOnValid makeSmosFileCursor rebuildSmosFileCursor
    describe "startSmosFile" $ it "is valid" $ shouldBeValid startSmosFile
    describe "smosFileCursorSelectedEntryL" $
        lensSpecOnValid smosFileCursorSelectedEntryL
    describe "smosFileCursorEntrySelectionL" $
        lensSpecOnValid smosFileCursorEntrySelectionL
    describe "smosFileCursorToggleHideEntireEntry" $
        it "produces valid cursors" $
        producesValidsOnValids smosFileCursorToggleHideEntireEntry
    describe "smosFileCursorSelectPrev" $
        it "produces valid cursors" $
        producesValidsOnValids smosFileCursorSelectPrev
    describe "smosFileCursorSelectNext" $
        it "produces valid cursors" $
        producesValidsOnValids smosFileCursorSelectNext
    describe "smosFileCursorInsertEntryBefore" $ do
        it "produces valid cursors" $
            producesValidsOnValids smosFileCursorInsertEntryBefore
        it "inserts an entry below the currently selected entry" pending
    describe "smosFileCursorInsertEntryBeforeAndSelectHeader" $ do
        it "produces valid cursors" $
            producesValidsOnValids
                smosFileCursorInsertEntryBeforeAndSelectHeader
        it "inserts an entry below the currently selected entry" pending
    describe "smosFileCursorInsertEntryBelow" $ do
        it "produces valid cursors" $
            producesValidsOnValids smosFileCursorInsertEntryBelow
        it "inserts an entry below the currently selected entry" pending
    describe "smosFileCursorInsertEntryBelowAndSelectHeader" $ do
        it "produces valid cursors" $
            producesValidsOnValids smosFileCursorInsertEntryBelowAndSelectHeader
        it "inserts an entry below the currently selected entry" pending
    describe "smosFileCursorInsertEntryAfter" $ do
        it "produces valid cursors" $
            producesValidsOnValids smosFileCursorInsertEntryAfter
        it "inserts an entry above the currently selected entry" pending
    describe "smosFileCursorInsertEntryAfterAndSelectHeader" $ do
        it "produces valid cursors" $
            producesValidsOnValids smosFileCursorInsertEntryAfterAndSelectHeader
        it "inserts an entry above the currently selected entry" pending
    describe "smosFileCursorSwapPrev" $
        it "produces valid cursors" $
        producesValidsOnValids smosFileCursorSwapPrev
    describe "smosFileCursorSwapNext" $
        it "produces valid cursors" $
        producesValidsOnValids smosFileCursorSwapNext
    describe "smosFileCursorPromoteElem" $
        it "produces valid cursors" $
        producesValidsOnValids smosFileCursorPromoteEntry
    describe "smosFileCursorPromoteSubTree" $
        it "produces valid cursors" $
        producesValidsOnValids smosFileCursorPromoteSubTree
    describe "smosFileCursorDemoteElem" $
        it "produces valid cursors" $
        producesValidsOnValids smosFileCursorDemoteEntry
    describe "smosFileCursorDemoteSubTree" $
        it "produces valid cursors" $
        producesValidsOnValids smosFileCursorDemoteSubTree
