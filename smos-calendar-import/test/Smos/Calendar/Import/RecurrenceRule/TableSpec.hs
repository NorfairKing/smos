module Smos.Calendar.Import.RecurrenceRule.TableSpec
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
spec =
  -- TODO add 'cover' clauses to make sure these tests have value
  describe "dailyNextRecurrence" $ do
    specify "ByMonth limits the recurrence set" $ forAllValid $ \cur ->
      forAll genDailyRecurrence $ \rrule -> do
        let limit = addLocalTime (30 * nominalDay) cur
        let withoutRule = rrule {rRuleByMonth = []}
        let with = rruleOccurrencesUntil cur rrule limit
        let without = rruleOccurrencesUntil cur rrule limit
        S.size with `shouldSatisfy` (<= S.size without)
    specify "ByMonthDay limits the recurrence set" $ forAllValid $ \cur ->
      forAll genDailyRecurrence $ \rrule -> do
        let limit = addLocalTime (30 * nominalDay) cur
        let withoutRule = rrule {rRuleByMonthDay = []}
        let with = rruleOccurrencesUntil cur rrule limit
        let without = rruleOccurrencesUntil cur rrule limit
        S.size with `shouldSatisfy` (<= S.size without)
    specify "ByDay limits the recurrence set" $ forAllValid $ \cur ->
      forAll genDailyRecurrence $ \rrule -> do
        let limit = addLocalTime (30 * nominalDay) cur
        let withoutRule = rrule {rRuleByDay = []}
        let with = rruleOccurrencesUntil cur rrule limit
        let without = rruleOccurrencesUntil cur rrule limit
        S.size with `shouldSatisfy` (<= S.size without)
    specify "ByHour expands the recurrence set" $ forAllValid $ \cur ->
      forAll genDailyRecurrence $ \rrule -> do
        let limit = addLocalTime (30 * nominalDay) cur
        let withoutRule = rrule {rRuleByHour = []}
        let with = rruleOccurrencesUntil cur rrule limit
        let without = rruleOccurrencesUntil cur rrule limit
        S.size with `shouldSatisfy` (>= S.size without)
    specify "ByMinute expands the recurrence set" $ forAllValid $ \cur ->
      forAll genDailyRecurrence $ \rrule -> do
        let limit = addLocalTime (30 * nominalDay) cur
        let withoutRule = rrule {rRuleByMinute = []}
        let with = rruleOccurrencesUntil cur rrule limit
        let without = rruleOccurrencesUntil cur rrule limit
        S.size with `shouldSatisfy` (>= S.size without)
    specify "BySecond expands the recurrence set" $ forAllValid $ \cur ->
      forAll genDailyRecurrence $ \rrule -> do
        let limit = addLocalTime (30 * nominalDay) cur
        let withoutRule = rrule {rRuleBySecond = []}
        let with = rruleOccurrencesUntil cur rrule limit
        let without = rruleOccurrencesUntil cur rrule limit
        S.size with `shouldSatisfy` (>= S.size without)
    specify "BySetPos limits the recurrence set" $ forAllValid $ \cur ->
      forAll genDailyRecurrence $ \rrule -> do
        let limit = addLocalTime (30 * nominalDay) cur
        let withoutRule = rrule {rRuleBySetPos = []}
        let with = rruleOccurrencesUntil cur rrule limit
        let without = rruleOccurrencesUntil cur rrule limit
        S.size with `shouldSatisfy` (<= S.size without)
