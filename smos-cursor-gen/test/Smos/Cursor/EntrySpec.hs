{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.EntrySpec where

import Test.Hspec
import Test.Validity
import Test.Validity.Optics

import Smos.Data.Gen ()

import Smos.Cursor.Entry
import Smos.Cursor.Entry.Gen ()

spec :: Spec
spec = do
    eqSpec @EntryCursor
    genValidSpec @EntryCursor
    eqSpec @EntryCursorSelection
    genValidSpec @EntryCursorSelection
    describe "makeEntryCursor" $
        it "produces valid cursors" $ producesValidsOnValids makeEntryCursor
    describe "emptyEntryCursor" $ it "is valid" $ shouldBeValid emptyEntryCursor
    describe "rebuildEntryCursor" $ do
        it "produces valid entries" $ producesValidsOnValids rebuildEntryCursor
        it "is the inverse of makeEntryCursor" $
            inverseFunctionsOnValid makeEntryCursor rebuildEntryCursor
    describe "entryCursorSelectionL" $ lensSpecOnValid entryCursorSelectionL
