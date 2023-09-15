{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Report.OngoingSpec where

import qualified Data.Map as M
import Data.Time
import Data.Time.Zones.All
import Smos.Data
import Smos.Directory.Archive.Gen ()
import Smos.Directory.ShouldPrint
import Smos.Directory.TestUtils
import Smos.Report.Filter.Gen ()
import Smos.Report.Ongoing
import Smos.Report.Ongoing.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @OngoingReport
  jsonSpec @OngoingReport
  genValidSpec @OngoingEntry
  jsonSpec @OngoingEntry
  modifyMaxSuccess (`div` 10) $
    describe "produceOngoingReport" $
      it "produces valid reports for interesting stores" $
        forAllValid $ \zone ->
          forAllValid $ \now ->
            forAllValid $ \mFilter ->
              forAllValid $ \ha ->
                withInterestingStore $ \dc -> do
                  wr <- produceOngoingReport zone now mFilter ha DontPrint dc
                  shouldBeValid wr

  describe "parseOngoingEntry" $ do
    it "always uses the appropriate filepath and header if it parses something" $
      forAllValid $ \zone ->
        forAllValid $ \now ->
          forAllValid $ \rf ->
            forAllValid $ \e -> do
              case parseOngoingEntry zone now rf e of
                Nothing -> pure () -- Fine
                Just OngoingEntry {..} -> do
                  ongoingEntryFilePath `shouldBe` rf
                  ongoingEntryHeader `shouldBe` entryHeader e

    let zone = tzByLabel Europe__Zurich
        now = UTCTime (fromGregorian 2023 09 11) (timeOfDayToTime (TimeOfDay 12 14 00))

    it "can parse a OnlyBegin" $
      forAllValid $ \rf ->
        forAllValid $ \h -> do
          let begin = TimestampDay (fromGregorian 2023 09 10)
              e = (newEntry h) {entryTimestamps = M.fromList [("BEGIN", begin)]}
          case parseOngoingEntry zone now rf e of
            Nothing -> expectationFailure "Should have found an OngoingEntry"
            Just OngoingEntry {..} -> ongoingEntryBeginEnd `shouldBe` OnlyBegin begin

    it "can parse a OnlyEnd" $
      forAllValid $ \rf ->
        forAllValid $ \h -> do
          let end = TimestampDay (fromGregorian 2023 09 15)
              e = (newEntry h) {entryTimestamps = M.fromList [("END", end)]}
          case parseOngoingEntry zone now rf e of
            Nothing -> expectationFailure "Should have found an OngoingEntry"
            Just OngoingEntry {..} -> ongoingEntryBeginEnd `shouldBe` OnlyEnd end

    it "can parse a OnlyEnd and considers the end date exclusive" $
      forAllValid $ \rf ->
        forAllValid $ \h -> do
          let end = TimestampDay (fromGregorian 2023 09 11)
              e = (newEntry h) {entryTimestamps = M.fromList [("END", end)]}
          case parseOngoingEntry zone now rf e of
            Nothing -> pure ()
            Just _ -> expectationFailure "Failed to consider the end date exclusive"

    it "can parse a BeginEnd" $
      forAllValid $ \rf ->
        forAllValid $ \h -> do
          let begin = TimestampDay (fromGregorian 2023 09 09)
              end = TimestampDay (fromGregorian 2023 09 16)
              e = (newEntry h) {entryTimestamps = M.fromList [("BEGIN", begin), ("END", end)]}
          case parseOngoingEntry zone now rf e of
            Nothing -> expectationFailure "Should have found an OngoingEntry"
            Just OngoingEntry {..} -> ongoingEntryBeginEnd `shouldBe` BeginEnd begin end

  describe "beginEndPercentageString" $ do
    it "considers the end date exclusive" $
      beginEndPercentageString
        (LocalTime (fromGregorian 2023 09 15) (TimeOfDay 10 56 00))
        (TimestampDay (fromGregorian 2023 09 15))
        (TimestampDay (fromGregorian 2023 09 16))
        `shouldBe` "  0 /   1"
