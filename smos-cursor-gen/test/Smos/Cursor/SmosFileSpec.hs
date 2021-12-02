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
spec = modifyMaxShrinks (const 0) $ do
  genValidSpec @SmosFileCursor
  lensSpec smosFileCursorForestCursorL
  lensSpec smosFileCursorSelectedEntryL
  describe "makeSmosFileCursor" $
    it "produces valid cursors" $
      producesValid makeSmosFileCursor
  describe "rebuildSmosFileCursor" $ do
    it "produces valid cursors" $ producesValid rebuildSmosFileCursor
    it "is the inverse of makeFileCursor" $
      inverseFunctions makeSmosFileCursor rebuildSmosFileCursor
  describe "startSmosFile" $ it "is valid" $ shouldBeValid startSmosFile
  describe "smosFileCursorReadyForStartup" $
    it "rebuilds to the same" $
      forAllValid $ \sfc ->
        rebuildSmosFileCursorEntirely (smosFileCursorReadyForStartup sfc) `shouldBe` rebuildSmosFileCursorEntirely sfc
  describe "smosFileCursorToggleHideEntireEntry" $
    it "produces valid cursors" $
      producesValid smosFileCursorToggleCollapseEntireEntry
  describe "smosFileCursorSelectPrev" $
    it "produces valid cursors" $
      producesValid smosFileCursorSelectPrev
  describe "smosFileCursorSelectNext" $
    it "produces valid cursors" $
      producesValid smosFileCursorSelectNext
  describe "smosFileCursorSelectFirst" $
    it "produces valid cursors" $
      producesValid smosFileCursorSelectFirst
  describe "smosFileCursorSelectLast" $
    it "produces valid cursors" $
      producesValid smosFileCursorSelectLast
  describe "smosFileCursorToggleCollapse" $
    it "produces valid cursors" $
      producesValid smosFileCursorToggleCollapse
  describe "smosFileCursorToggleCollapseRecursively" $
    it "produces valid cursors" $
      producesValid smosFileCursorToggleCollapseRecursively
  describe "smosFileCursorInsertEntryBefore" $ do
    it "produces valid cursors" $ producesValid smosFileCursorInsertEntryBefore
    it "produces a cursor in which you can select the next" $
      forAllValid $ \sfc ->
        let sfc' = smosFileCursorInsertEntryBefore sfc
         in smosFileCursorSelectPrev sfc' `shouldSatisfy` isJust
    pending "inserts an entry below the currently selected entry"
  describe "smosFileCursorInsertEntryBeforeAndSelect" $ do
    it "produces valid cursors" $
      producesValid smosFileCursorInsertEntryBeforeAndSelect
    pending "inserts an entry below the currently selected entry"
  describe "smosFileCursorInsertEntryBelowAtStart" $ do
    it "produces valid cursors" $ producesValid smosFileCursorInsertEntryBelowAtStart
    it "produces a cursor in which you can select below" $
      forAllValid $ \sfc ->
        let sfc' = smosFileCursorInsertEntryBelowAtStart sfc
         in smosFileCursorSelectBelowAtStart sfc' `shouldSatisfy` isJust
    pending "inserts an entry below the currently selected entry"
  describe "smosFileCursorInsertEntryBelowAtStartAndSelect" $ do
    it "produces valid cursors" $
      producesValid smosFileCursorInsertEntryBelowAtStartAndSelect
    pending "inserts an entry below the currently selected entry"
  describe "smosFileCursorInsertEntryBelowAtEnd" $ do
    it "produces valid cursors" $ producesValid smosFileCursorInsertEntryBelowAtEnd
    it "produces a cursor in which you can select below" $
      forAllValid $ \sfc ->
        let sfc' = smosFileCursorInsertEntryBelowAtEnd sfc
         in smosFileCursorSelectBelowAtEnd sfc' `shouldSatisfy` isJust
    pending "inserts an entry below the currently selected entry"
  describe "smosFileCursorInsertEntryBelowAtEndAndSelect" $ do
    it "produces valid cursors" $
      producesValid smosFileCursorInsertEntryBelowAtEndAndSelect
    pending "inserts an entry below the currently selected entry"
  describe "smosFileCursorInsertEntryAfter" $ do
    it "produces valid cursors" $ producesValid smosFileCursorInsertEntryAfter
    it "produces a cursor in which you can select the next" $
      forAllValid $ \sfc ->
        let sfc' = smosFileCursorInsertEntryAfter sfc
         in smosFileCursorSelectNext sfc' `shouldSatisfy` isJust
    pending "inserts an entry above the currently selected entry"
  describe "smosFileCursorInsertEntryAfterAndSelect" $ do
    it "produces valid cursors" $
      producesValid smosFileCursorInsertEntryAfterAndSelect
    pending "inserts an entry above the currently selected entry"
  describe "smosFileCursorSwapPrev" $
    it "produces valid cursors" $
      producesValid smosFileCursorSwapPrev
  describe "smosFileCursorSwapNext" $
    it "produces valid cursors" $
      producesValid smosFileCursorSwapNext
  describe "smosFileCursorPromoteElem" $
    it "produces valid cursors" $
      producesValid smosFileCursorPromoteEntry
  describe "smosFileCursorPromoteSubTree" $
    it "produces valid cursors" $
      producesValid smosFileCursorPromoteSubTree
  describe "smosFileCursorDemoteElem" $
    it "produces valid cursors" $
      producesValid smosFileCursorDemoteEntry
  describe "smosFileCursorDemoteSubTree" $
    it "produces valid cursors" $
      producesValid smosFileCursorDemoteSubTree
  describe "smosFileCursorClockOutEverywhere" $
    it "produces valid cursors" $
      producesValid2 smosFileCursorClockOutEverywhere
  describe "smosFileCursorClockOutEverywhereAndClockInHere" $
    it "produces valid cursors" $
      producesValid2 smosFileCursorClockOutEverywhereAndClockInHere
  describe "smosFileSubtreeSetTodoState" $ do
    it "produces valid cursors when unsetting todo states" $
      forAllValid $
        \now -> producesValid $ smosFileSubtreeSetTodoState now Nothing
    it "produces valid cursors" $ producesValid3 smosFileSubtreeSetTodoState
