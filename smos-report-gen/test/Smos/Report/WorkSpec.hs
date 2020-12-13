{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Report.WorkSpec where

import qualified Data.DirForest as DF
import Data.GenValidity.Path ()
import qualified Data.Map as M
import qualified Data.Set as S
import Path
import Smos.Data
import Smos.Data.Gen ()
import Smos.Report.Archive
import Smos.Report.Archive.Gen ()
import Smos.Report.Config
import Smos.Report.Filter.Gen ()
import Smos.Report.InterestingStore
import Smos.Report.ShouldPrint
import Smos.Report.TestUtils
import Smos.Report.Work
import Smos.Report.Work.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @WorkReportContext
  genValidSpec @IntermediateWorkReport
  genValidSpec @WorkReport
  describe "makeIntermediateWorkReport" $
    it "produces valid intermediate work reports" $
      producesValidsOnValids3 makeIntermediateWorkReport
  describe "finishWorkReport" $
    it "produces valid work reports" $
      forAllValid $ \zt ->
        forAllValid $ \pn ->
          forAllValid $ \mt ->
            forAllValid $ \ms ->
              forAllValid $ \workReport ->
                shouldBeValid $ finishWorkReport zt pn mt ms workReport

  modifyMaxSuccess (`div` 10) $
    describe "produceWorkReport" $ do
      it "finds next actions even if there is no time property or filter and no contexts" $
        forAllValid $ \now ->
          forAllValid $ \ts ->
            let rf = [relfile|example.smos|]
                e =
                  emptyEntry
                    { entryStateHistory =
                        StateHistory
                          [ StateHistoryEntry
                              { stateHistoryEntryNewState = Just "NEXT",
                                stateHistoryEntryTimestamp = ts
                              }
                          ]
                    }
                is =
                  emptyInterestingStore
                    { workflowFiles = DF.singletonFile rf (SmosFile [Node e []])
                    }
             in withDirectoryConfig is $ \dc -> do
                  let ctx =
                        WorkReportContext
                          { workReportContextNow = now,
                            workReportContextProjectsSubdir = Just [reldir|projects|],
                            workReportContextBaseFilter = Just defaultWorkBaseFilter,
                            workReportContextCurrentContext = Nothing,
                            workReportContextTimeProperty = Nothing,
                            workReportContextTime = Nothing,
                            workReportContextAdditionalFilter = Nothing,
                            workReportContextContexts = M.empty,
                            workReportContextChecks = S.empty,
                            workReportContextSorter = Nothing,
                            workReportContextWaitingThreshold = 7,
                            workReportContextStuckThreshold = 21
                          }
                  wr <- produceWorkReport HideArchive DontPrint dc ctx
                  case workReportResultEntries wr of
                    [] -> expectationFailure "No results found"
                    (rp, _) : _ -> do
                      rp `shouldBe` rf
      it "finds next actions even if there is a next meeting but no time property on the next actions" $
        forAllValid $ \now ->
          forAllValid $ \ts1 ->
            forAllValid $ \ts2 ->
              let rf = [relfile|example.smos|]
                  e1 =
                    emptyEntry
                      { entryStateHistory =
                          StateHistory
                            [ StateHistoryEntry
                                { stateHistoryEntryNewState = Just "NEXT",
                                  stateHistoryEntryTimestamp = ts1
                                }
                            ]
                      }
                  e2 = emptyEntry {entryTimestamps = M.singleton "BEGIN" ts2}
                  is =
                    emptyInterestingStore
                      { workflowFiles = DF.singletonFile rf (SmosFile [Node e1 [], Node e2 []])
                      }
               in withDirectoryConfig is $ \dc -> do
                    let ctx =
                          WorkReportContext
                            { workReportContextNow = now,
                              workReportContextProjectsSubdir = Just [reldir|projects|],
                              workReportContextBaseFilter = Just defaultWorkBaseFilter,
                              workReportContextCurrentContext = Nothing,
                              workReportContextTimeProperty = Nothing,
                              workReportContextTime = Nothing,
                              workReportContextAdditionalFilter = Nothing,
                              workReportContextContexts = M.empty,
                              workReportContextChecks = S.empty,
                              workReportContextSorter = Nothing,
                              workReportContextWaitingThreshold = 7,
                              workReportContextStuckThreshold = 21
                            }
                    wr <- produceWorkReport HideArchive DontPrint dc ctx
                    case workReportResultEntries wr of
                      [] -> expectationFailure "No results found"
                      (rp, _) : _ -> do
                        rp `shouldBe` rf
      it "produces valid reports for interesting stores" $
        forAllValid $
          \wrc ->
            forAllValid $ \ha ->
              withInterestingStore $ \dc -> do
                nar <- produceWorkReport ha DontPrint dc wrc
                shouldBeValid nar
