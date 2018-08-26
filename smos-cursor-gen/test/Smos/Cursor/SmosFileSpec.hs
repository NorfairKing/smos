{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.SmosFileSpec where

import Test.Hspec
import Test.Validity

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
    describe "smosFileCursorInsertEntryAfter" $ do
        it "produces valid cursors" $
            producesValidsOnValids smosFileCursorInsertEntryAfter
        it "inserts an entry above the currently selected entry" pending
    describe "smosFileCursorInsertEntryAfterAndSelectHeader" $ do
        it "produces valid cursors" $
            producesValidsOnValids smosFileCursorInsertEntryAfterAndSelectHeader
        it "inserts an entry above the currently selected entry" pending
    describe "smosFileCursorInsertEntryBefore" $ do
        it "produces valid cursors" $
            producesValidsOnValids smosFileCursorInsertEntryBefore
        it "inserts an entry below the currently selected entry" pending
    describe "smosFileCursorInsertEntryBeforeAndSelectHeader" $ do
        it "produces valid cursors" $
            producesValidsOnValids smosFileCursorInsertEntryBeforeAndSelectHeader
        it "inserts an entry below the currently selected entry" pending
