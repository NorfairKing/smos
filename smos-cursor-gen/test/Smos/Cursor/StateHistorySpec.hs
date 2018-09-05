{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.StateHistorySpec where

import Test.Hspec
import Test.Validity

import Smos.Data.Gen ()

import Smos.Cursor.StateHistory
import Smos.Cursor.StateHistory.Gen ()

spec :: Spec
spec = do
    eqSpec @StateHistoryCursor
    genValidSpec @StateHistoryCursor
    describe "makeStateHistoryCursor" $
        it "produces valid cursors" $
        producesValidsOnValids makeStateHistoryCursor
    describe "rebuildStateHistoryCursor" $ do
        it "produces valid cursors" $
            producesValidsOnValids rebuildStateHistoryCursor
        it "is the inverse of makeStateHistoryCursor" $
            inverseFunctionsOnValid
                makeStateHistoryCursor
                rebuildStateHistoryCursor
    describe "stateHistoryCursorSetTodoState" $ do
        it "produces valid cursors" $
            producesValidsOnValids3 stateHistoryCursorSetTodoState
        it "sets the given todo state" pending
    describe "stateHistoryCursorToggleTodoState" $ do
        it "produces valid cursors" $
            producesValidsOnValids3 stateHistoryCursorToggleTodoState
        it "toggles the given todo state" pending
    describe "stateHistoryCursorUnsetTodoState" $ do
        it "produces valid cursors" $
            producesValidsOnValids2 stateHistoryCursorUnsetTodoState
        it "unsets any todo state" pending
