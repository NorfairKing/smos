{-# LANGUAGE RecordWildCards #-}

module Smos.Report.ClockSpec where

import Data.GenValidity.Path ()
import Data.Time
import Smos.Data
import Smos.Data.Gen ()
import Smos.Report.Clock
import Smos.Report.Clock.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.Period
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "zeroOutByFilter" $
    it "produces valid smos files" $
      producesValid3 zeroOutByFilter
  describe "trimLogbookEntry" $
    it "produces valid logbook entries" $
      producesValid3 trimLogbookEntry
  describe "trimLogbookEntryToInterval" $ do
    it "produces valid logbook entries" $
      producesValid3
        trimLogbookEntryToInterval
    it "leaves an entry that is within the interval" $
      let zone = utc
       in forAllValid $ \interval ->
            forAllValid $ \logbookEntry ->
              case trimLogbookEntryToInterval zone interval logbookEntry of
                Nothing -> pure () -- Fine
                Just LogbookEntry {..} -> do
                  localDay (utcToLocalTime zone logbookEntryStart) `shouldSatisfy` filterInterval interval
                  localDay (utcToLocalTime zone logbookEntryEnd) `shouldSatisfy` filterInterval interval

  describe "sumLogbookEntryTime" $
    it "produces valid difftimes" $
      producesValid sumLogbookEntryTime
