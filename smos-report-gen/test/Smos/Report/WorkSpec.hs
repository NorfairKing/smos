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
import Smos.Directory.Archive
import Smos.Directory.Archive.Gen ()
import Smos.Directory.InterestingStore
import Smos.Directory.ShouldPrint
import Smos.Directory.TestUtils
import Smos.Report.Filter
import Smos.Report.Filter.Gen ()
import Smos.Report.OptParse (defaultWorkBaseFilter)
import Smos.Report.Time
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
      producesValid3 makeIntermediateWorkReport
  describe "finishWorkReport" $
    it "produces valid work reports" $
      forAllValid $ \zone ->
        forAllValid $ \now ->
          forAllValid $ \pn ->
            forAllValid $ \mt ->
              forAllValid $ \ms ->
                forAllValid $ \workReport ->
                  shouldBeValid $ finishWorkReport zone now pn mt ms workReport

  modifyMaxSuccess (`div` 10) $
    describe "produceWorkReport" $ do
      it "produces valid reports for interesting stores" $
        forAllValid $ \wrc ->
          forAllValid $ \ha ->
            withInterestingStore $ \dc -> do
              nar <- produceWorkReport ha DontPrint dc wrc
              shouldBeValid nar
      it "finds next actions even if there is no time property or filter and no contexts" $
        forAllValid $ \zone ->
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
                      { workflowFiles = DF.singletonFile rf (makeSmosFile [Node e []])
                      }
               in withDirectorySettings is $ \dc -> do
                    let ctx =
                          WorkReportContext
                            { workReportContextTimeZone = zone,
                              workReportContextNow = now,
                              workReportContextProjectsSubdir = Just [reldir|projects|],
                              workReportContextBaseFilter = Just defaultWorkBaseFilter,
                              workReportContextCurrentContext = Nothing,
                              workReportContextTimeProperty = Nothing,
                              workReportContextTime = Nothing,
                              workReportContextAdditionalFilter = Nothing,
                              workReportContextContexts = M.empty,
                              workReportContextChecks = S.empty,
                              workReportContextSorter = Nothing,
                              workReportContextWaitingThreshold = Days 7,
                              workReportContextStuckThreshold = Weeks 3
                            }
                    wr <- produceWorkReport HideArchive DontPrint dc ctx
                    case workReportResultEntries wr of
                      [] -> expectationFailure "No results found"
                      (rp, _) : _ -> do
                        rp `shouldBe` rf
      it "finds next actions even if there is a next meeting but no time property on the next actions" $
        forAllValid $ \zone ->
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
                        { workflowFiles = DF.singletonFile rf (makeSmosFile [Node e1 [], Node e2 []])
                        }
                 in withDirectorySettings is $ \dc -> do
                      let ctx =
                            WorkReportContext
                              { workReportContextTimeZone = zone,
                                workReportContextNow = now,
                                workReportContextProjectsSubdir = Just [reldir|projects|],
                                workReportContextBaseFilter = Just defaultWorkBaseFilter,
                                workReportContextCurrentContext = Nothing,
                                workReportContextTimeProperty = Nothing,
                                workReportContextTime = Nothing,
                                workReportContextAdditionalFilter = Nothing,
                                workReportContextContexts = M.empty,
                                workReportContextChecks = S.empty,
                                workReportContextSorter = Nothing,
                                workReportContextWaitingThreshold = Days 7,
                                workReportContextStuckThreshold = Weeks 3
                              }
                      wr <- produceWorkReport HideArchive DontPrint dc ctx
                      case workReportResultEntries wr of
                        [] -> expectationFailure "No results found"
                        (rp, _) : _ -> do
                          rp `shouldBe` rf
      it "finds check failures actions even if there are no contexts" $
        forAllValid $ \zone ->
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
                      { workflowFiles = DF.singletonFile rf (makeSmosFile [Node e []])
                      }
                  checkFilterString = "property:timewindow"
               in case parseEntryFilter checkFilterString of
                    Left err -> expectationFailure $ show err
                    Right checkFilter -> withDirectorySettings is $ \dc -> do
                      let ctx =
                            WorkReportContext
                              { workReportContextTimeZone = zone,
                                workReportContextNow = now,
                                workReportContextProjectsSubdir = Just [reldir|projects|],
                                workReportContextBaseFilter = Just defaultWorkBaseFilter,
                                workReportContextCurrentContext = Nothing,
                                workReportContextTimeProperty = Nothing,
                                workReportContextTime = Nothing,
                                workReportContextAdditionalFilter = Nothing,
                                workReportContextContexts = M.empty,
                                workReportContextChecks = S.singleton checkFilter,
                                workReportContextSorter = Nothing,
                                workReportContextWaitingThreshold = Days 7,
                                workReportContextStuckThreshold = Weeks 3
                              }
                      wr <- produceWorkReport HideArchive DontPrint dc ctx
                      case M.toList $ workReportCheckViolations wr of
                        [] -> expectationFailure "No check violations found."
                        (cf, _) : _ -> do
                          cf `shouldBe` checkFilter
