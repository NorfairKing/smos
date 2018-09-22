{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.CollapseSpec where

import Test.Hspec
import Test.Validity
import Test.Validity.Optics

import Smos.Data.Gen ()

import Smos.Cursor.Collapse
import Smos.Cursor.Collapse.Gen ()

spec :: Spec
spec = do
    eqSpecOnValid @(CollapseEntry Double)
    genValidSpec @(CollapseEntry Double)
    describe "makeCollapseEntry" $
        it "produces valid collapse's" $
        producesValidsOnValids (makeCollapseEntry @Double)
    describe "rebuildCollapseEntry" $ do
        it "produces valid values" $
            producesValidsOnValids (rebuildCollapseEntry @Double)
        it "is the inverse of makeCollapseEntry" $
            inverseFunctionsOnValid
                (makeCollapseEntry @Double)
                (rebuildCollapseEntry @Double)
    describe "collapseEntryValueL" $
        lensSpecOnValid (collapseEntryValueL @Double)
    describe "collapseEntryShowContentsL" $
        lensSpecOnValid (collapseEntryShowContentsL @Double)
    describe "collapseEntryShowHistoryL" $
        lensSpecOnValid (collapseEntryShowHistoryL @Double)
    describe "collapseEntrySetShowAll" $
        it "produces valid collapses" $
        producesValidsOnValids2 (collapseEntrySetShowAll @Double)
