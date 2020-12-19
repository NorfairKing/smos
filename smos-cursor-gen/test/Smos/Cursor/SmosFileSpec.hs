{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.SmosFileSpec where

import Data.Maybe
import Smos.Cursor.SmosFile
import Smos.Cursor.SmosFile.Gen ()
import Smos.Data.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Lens

spec :: Spec
spec = modifyMaxShrinks (const 1) $ do
  genValidSpec @SmosFileCursor
  lensSpecOnValid smosFileCursorForestCursorL
  lensSpecOnValid smosFileCursorSelectedEntryL
  describe "makeSmosFileCursor" $
    it "produces valid cursors" $
      producesValidsOnValids makeSmosFileCursor
  describe "rebuildSmosFileCursor" $ do
    it "produces valid cursors" $ producesValidsOnValids rebuildSmosFileCursor
    it "is the inverse of makeFileCursor" $
      inverseFunctionsOnValid makeSmosFileCursor rebuildSmosFileCursor
  describe "startSmosFile" $ it "is valid" $ shouldBeValid startSmosFile
  describe "smosFileCursorToggleHideEntireEntry" $
    it "produces valid cursors" $
      producesValidsOnValids smosFileCursorToggleCollapseEntireEntry
  describe "smosFileCursorSelectPrev" $
    it "produces valid cursors" $
      producesValidsOnValids smosFileCursorSelectPrev
  describe "smosFileCursorSelectNext" $
    it "produces valid cursors" $
      producesValidsOnValids smosFileCursorSelectNext
  describe "smosFileCursorSelectFirst" $
    it "produces valid cursors" $
      producesValidsOnValids smosFileCursorSelectFirst
  describe "smosFileCursorSelectLast" $
    it "produces valid cursors" $
      producesValidsOnValids smosFileCursorSelectLast
  describe "smosFileCursorToggleCollapse" $
    it "produces valid cursors" $
      producesValidsOnValids smosFileCursorToggleCollapse
  describe "smosFileCursorToggleCollapseRecursively" $
    it "produces valid cursors" $
      producesValidsOnValids smosFileCursorToggleCollapseRecursively
  describe "smosFileCursorInsertEntryBefore" $ do
    it "produces valid cursors" $ producesValidsOnValids smosFileCursorInsertEntryBefore
    it "produces a cursor in which you can select the next" $
      forAllValid $ \sfc ->
        let sfc' = smosFileCursorInsertEntryBefore sfc
         in smosFileCursorSelectPrev sfc' `shouldSatisfy` isJust
    pending "inserts an entry below the currently selected entry"
  describe "smosFileCursorInsertEntryBeforeAndSelect" $ do
    it "produces valid cursors" $
      producesValidsOnValids smosFileCursorInsertEntryBeforeAndSelect
    pending "inserts an entry below the currently selected entry"
  describe "smosFileCursorInsertEntryBelowAtStart" $ do
    it "produces valid cursors" $ producesValidsOnValids smosFileCursorInsertEntryBelowAtStart
    it "produces a cursor in which you can select below" $
      forAllValid $ \sfc ->
        let sfc' = smosFileCursorInsertEntryBelowAtStart sfc
         in smosFileCursorSelectBelowAtStart sfc' `shouldSatisfy` isJust
    pending "inserts an entry below the currently selected entry"
  describe "smosFileCursorInsertEntryBelowAtStartAndSelect" $ do
    it "produces valid cursors" $
      producesValidsOnValids smosFileCursorInsertEntryBelowAtStartAndSelect
    pending "inserts an entry below the currently selected entry"
  describe "smosFileCursorInsertEntryBelowAtEnd" $ do
    it "produces valid cursors" $ producesValidsOnValids smosFileCursorInsertEntryBelowAtEnd
    it "produces a cursor in which you can select below" $
      forAllValid $ \sfc ->
        let sfc' = smosFileCursorInsertEntryBelowAtEnd sfc
         in smosFileCursorSelectBelowAtEnd sfc' `shouldSatisfy` isJust
    pending "inserts an entry below the currently selected entry"
  describe "smosFileCursorInsertEntryBelowAtEndAndSelect" $ do
    it "produces valid cursors" $
      producesValidsOnValids smosFileCursorInsertEntryBelowAtEndAndSelect
    pending "inserts an entry below the currently selected entry"
  describe "smosFileCursorInsertEntryAfter" $ do
    it "produces valid cursors" $ producesValidsOnValids smosFileCursorInsertEntryAfter
    it "produces a cursor in which you can select the next" $
      forAllValid $ \sfc ->
        let sfc' = smosFileCursorInsertEntryAfter sfc
         in smosFileCursorSelectNext sfc' `shouldSatisfy` isJust
    pending "inserts an entry above the currently selected entry"
  describe "smosFileCursorInsertEntryAfterAndSelect" $ do
    it "produces valid cursors" $
      producesValidsOnValids smosFileCursorInsertEntryAfterAndSelect
    pending "inserts an entry above the currently selected entry"
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
  describe "smosFileCursorClockOutEverywhere" $
    it "produces valid cursors" $
      producesValidsOnValids2 smosFileCursorClockOutEverywhere
  describe "smosFileCursorClockOutEverywhereAndClockInHere" $
    it "produces valid cursors" $
      producesValidsOnValids2 smosFileCursorClockOutEverywhereAndClockInHere
  describe "smosFileSubtreeSetTodoState" $ do
    it "produces valid cursors when unsetting todo states" $
      forAllValid $
        \now -> producesValidsOnValids $ smosFileSubtreeSetTodoState now Nothing
    it "produces valid cursors" $ producesValidsOnValids3 smosFileSubtreeSetTodoState
