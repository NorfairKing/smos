{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.RecurrenceRuleSpec
  ( spec,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Time
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurrenceRule.Gen
import Smos.Calendar.Import.UnresolvedTimestamp
import Test.Hspec
import Test.QuickCheck
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @Frequency
  genValidSpec @UntilCount
  genValidSpec @Interval
  genValidSpec @BySecond
  genValidSpec @ByMinute
  genValidSpec @ByHour
  genValidSpec @ByDay
  genValidSpec @ByMonthDay
  genValidSpec @ByYearDay
  genValidSpec @ByWeekNo
  genValidSpec @Month
  genValidSpec @BySetPos
  genValidSpec @RRule
  describe "genDailyRecurrence" $ it "generates valid rules" $ genGeneratesValid genDailyRecurrence
  xdescribe "These produces sets that are too big to be useful tests" $ do
    describe "rruleNextOccurrence" $ do
      it "produces valid results" $ producesValidsOnValids3 rruleNextOccurrence
    describe "dailyNextRecurrence" $ do
      it "produces valid results" $ forAllValid $ \cur ->
        forAllValid $ \limit ->
          forAllValid $ \int ->
            forAllValid $ \byMonths ->
              forAllValid $ \byMonthDays ->
                forAllValid $ \byDays ->
                  forAllValid $ \byHours ->
                    forAllValid $ \byMinutes ->
                      forAllValid $ \bySeconds ->
                        forAllValid $ \bySetPoss ->
                          shouldBeValid $
                            dailyNextRecurrence
                              cur
                              limit
                              int
                              byMonths
                              byMonthDays
                              byDays
                              byHours
                              byMinutes
                              bySeconds
                              bySetPoss
  describe "rruleOccurrencesUntil" $ do
    xit "produces valid results" $ producesValidsOnValids3 rruleOccurrencesUntil
    xit "produces results within the 'Count' range for 'Count' rules" $ forAllValid $ \start ->
      forAllValid $ \limit ->
        forAllValid $ \c ->
          forAll (((\r -> r {rRuleUntilCount = Count c}) <$> genValid) `suchThat` isValid) $ \rule ->
            let s = rruleOccurrencesUntil start rule limit
             in fromIntegral (S.size s) `shouldSatisfy` (<= c)
