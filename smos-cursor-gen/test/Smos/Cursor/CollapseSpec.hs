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
    eqSpecOnValid @(CollapseTree Double)
    genValidSpec @(CollapseTree Double)
    eqSpecOnValid @(Collapse Double)
    genValidSpec @(Collapse Double)
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
    describe "makeCollapseTree" $
        it "produces valid collapse's" $
        producesValidsOnValids (makeCollapseTree @Double)
    describe "rebuildCollapseTree" $ do
        it "produces valid values" $
            producesValidsOnValids (rebuildCollapseTree @Double)
        it "is the inverse of makeCollapseTree" $
            inverseFunctionsOnValid
                (makeCollapseTree @Double)
                (rebuildCollapseTree @Double)
    describe "collapseTreeValueL" $ lensSpecOnValid (collapseTreeValueL @Double)
    describe "collapseTreeShowSubForestL" $
        lensSpecOnValid (collapseTreeShowSubForestL @Double)
    describe "makeCollapse" $
        it "produces valid collapse's" $
        producesValidsOnValids (makeCollapse @Double)
    describe "rebuildCollapse" $ do
        it "produces valid values" $
            producesValidsOnValids (rebuildCollapse @Double)
        it "is the inverse of makeCollapse" $
            inverseFunctionsOnValid
                (makeCollapse @Double)
                (rebuildCollapse @Double)
    describe "collapseValueL" $ lensSpecOnValid (collapseValueL @Double)
    describe "collapseCollapseEntryL" $
        lensSpecOnValid (collapseCollapseEntryL @Double)
    describe "collapseShowSubForestL" $
        lensSpecOnValid (collapseShowSubForestL @Double)
    describe "collapseShowContentsL" $
        lensSpecOnValid (collapseShowContentsL @Double)
