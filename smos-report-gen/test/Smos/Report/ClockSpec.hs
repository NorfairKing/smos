{-# LANGUAGE RecordWildCards #-}

module Smos.Report.ClockSpec where

import Data.GenValidity.Path ()
import Smos.Data
import Smos.Data.Gen ()
import Smos.Report.Clock
import Smos.Report.Filter.Gen ()
import Smos.Report.Period
import Smos.Report.Period.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "zeroOutByFilter" $
    it "produces valid smos files" $
      producesValid3 zeroOutByFilter
  describe "trimLogbookEntryToInterval" $ do
    it "produces valid logbook entries" $
      producesValid3 trimLogbookEntryToInterval
    it "leaves an entry that is within the interval" $
      forAllValid $ \zone ->
        forAllValid $ \interval ->
          forAllValid $ \logbookEntry ->
            case trimLogbookEntryToInterval zone interval logbookEntry of
              Nothing -> pure () -- Fine
              Just LogbookEntry {..} -> do
                logbookEntryStart `shouldSatisfy` filterIntervalUTCTime zone interval
                logbookEntryEnd `shouldSatisfy` filterIntervalUTCTime zone interval

  describe "sumLogbookEntryTime" $
    it "produces valid difftimes" $
      producesValid sumLogbookEntryTime
