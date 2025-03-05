{-# LANGUAGE OverloadedStrings #-}

module Smos.Scheduler.HistorySpec (spec) where

import Data.Time.Zones
import Smos.Directory.TestUtils
import Smos.Scheduler.History
import Smos.Scheduler.History.Gen ()
import Test.Syd
import Test.Syd.Validity hiding (check)

spec :: Spec
spec = do
  describe "readReccurrenceHistory" $
    modifyMaxSuccess (`div` 10) $
      it "does not crash" $
        withInterestingStore $ \dc -> do
          history <- readReccurrenceHistory dc utcTZ
          shouldBeValid history

  describe "computeLastRun" $
    it "produces valid times" $
      producesValid2 computeLastRun

  describe "addScheduleMetadata" $
    it "produces valid times" $
      producesValid3 addScheduleMetadata

  describe "parseSmosFileSchedule" $
    it "can parse schedule hash that was added with addScheduleMetadata" $
      forAllValid $ \lt ->
        forAllValid $ \sf ->
          forAllValid $ \sih -> do
            parseSmosFileScheduleMetadata (addScheduleMetadata lt sih sf) `shouldBe` Just (sih, Just lt)
