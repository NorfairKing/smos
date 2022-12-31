{-# LANGUAGE TypeApplications #-}

module Smos.Report.StuckSpec where

import Data.GenValidity.Path ()
import Data.Time.Zones
import Data.Tree
import Smos.Data
import Smos.Data.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.Stuck
import Smos.Report.Stuck.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @StuckReport
  describe "makeStuckReportEntry" $
    it "produces valid reports" $
      producesValid3 makeStuckReportEntry
  describe "latestEntryInSmosFile" $ do
    it "selects a valid entry" $ producesValid2 latestEntryInSmosFile
    it "works for this example with an entry without state" $
      forAllValid $ \h1 ->
        forAllValid $ \h2 ->
          forAllValid $ \mts ->
            forAllValid $ \t ->
              let e1 =
                    (newEntry h1)
                      { entryStateHistory =
                          StateHistory
                            [ StateHistoryEntry
                                { stateHistoryEntryNewState = mts,
                                  stateHistoryEntryTimestamp = t
                                }
                            ]
                      }
                  e2 = newEntry h2
                  sf = makeSmosFile [Node e1 [], Node e2 []]
               in latestEntryInSmosFile utcTZ sf `shouldBe` Just e1
    it "works for this example with two entries with the same modification time. Then it uses the latter" $
      forAllValid $ \h1 ->
        forAllValid $ \h2 ->
          forAllValid $ \mts1 ->
            forAllValid $ \mts2 ->
              forAllValid $ \t ->
                let e1 =
                      (newEntry h1)
                        { entryStateHistory =
                            StateHistory
                              [ StateHistoryEntry
                                  { stateHistoryEntryNewState = mts1,
                                    stateHistoryEntryTimestamp = t
                                  }
                              ]
                        }
                    e2 =
                      (newEntry h2)
                        { entryStateHistory =
                            StateHistory
                              [ StateHistoryEntry
                                  { stateHistoryEntryNewState = mts2,
                                    stateHistoryEntryTimestamp = t
                                  }
                              ]
                        }
                    sf = makeSmosFile [Node e1 [], Node e2 []]
                 in latestEntryInSmosFile utcTZ sf `shouldBe` Just e2
  describe "latestStateChange" $
    it "selects a valid entry" $
      producesValid latestStateChange
  describe "makeStuckReport" $ it "produces valid reports" $ producesValid makeStuckReport
