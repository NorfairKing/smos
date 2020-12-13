{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.StateHistorySpec where

import Smos.Cursor.StateHistory
import Smos.Cursor.StateHistory.Gen ()
import Smos.Data.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @StateHistoryCursor
  describe "makeStateHistoryCursor" $
    it "produces valid cursors" $
      producesValidsOnValids makeStateHistoryCursor
  describe "rebuildStateHistoryCursor" $ do
    it "produces valid cursors" $ producesValidsOnValids rebuildStateHistoryCursor
    it "is the inverse of makeStateHistoryCursor" $
      inverseFunctionsOnValid makeStateHistoryCursor rebuildStateHistoryCursor
  describe "stateHistoryCursorSetTodoState" $ do
    it "produces valid cursors" $ producesValidsOnValids3 stateHistoryCursorSetTodoState
    pending "sets the given todo state"
  describe "stateHistoryCursorToggleTodoState" $ do
    it "produces valid cursors" $ producesValidsOnValids3 stateHistoryCursorToggleTodoState
    pending "toggles the given todo state"
  describe "stateHistoryCursorUnsetTodoState" $ do
    it "produces valid cursors" $ producesValidsOnValids2 stateHistoryCursorUnsetTodoState
    pending "unsets any todo state"
