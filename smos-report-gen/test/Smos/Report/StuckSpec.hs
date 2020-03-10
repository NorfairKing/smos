{-# LANGUAGE TypeApplications #-}

module Smos.Report.StuckSpec where

import Test.Hspec
import Test.Validity

import Data.Tree

import Data.GenValidity.Path ()

import Smos.Data
import Smos.Data.Gen ()

import Smos.Report.Stuck

import Smos.Report.Filter.Gen ()
import Smos.Report.Stuck.Gen ()

spec :: Spec
spec = do
  genValidSpec @StuckReport
  describe "makeStuckReportEntry" $
    it "produces valid reports" $ producesValidsOnValids2 makeStuckReportEntry
  describe "latestEntryInSmosFile" $ do
    it "selects a valid entry" $ producesValidsOnValids latestEntryInSmosFile
    it "works for this example with an entry without state" $
      forAllValid $ \(h1, h2) ->
        forAllValid $ \(mts, t) ->
          let e1 =
                (newEntry h1)
                  { entryStateHistory =
                      StateHistory
                        [ StateHistoryEntry
                            {stateHistoryEntryNewState = mts, stateHistoryEntryTimestamp = t}
                        ]
                  }
              e2 = newEntry h2
              sf = SmosFile [Node e1 [], Node e2 []]
           in latestEntryInSmosFile sf `shouldBe` Just e1
  describe "latestStateChange" $
    it "selects a valid entry" $ producesValidsOnValids latestStateChange
