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
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity
import Text.Show.Pretty

spec :: Spec
spec = modifyMaxSize (`div` 3) -- There's no point in getting sets that are too big, these are just smoke tests anyway
  $ describe "dailyNextRecurrence"
  -- TODO add 'cover' clauses to make sure these tests have value
  $ do
    -- > +----------+-------+
    -- > |          |DAILY  |
    -- > +----------+-------+
    -- > |BYMONTH   |Limit  |
    -- > +----------+-------+
    -- > |BYWEEKNO  |N/A    |
    -- > +----------+-------+
    -- > |BYYEARDAY |N/A    |
    -- > +----------+-------+
    -- > |BYMONTHDAY|Limit  |
    -- > +----------+-------+
    -- > |BYDAY     |Limit  |
    -- > +----------+-------+
    -- > |BYHOUR    |Expand |
    -- > +----------+-------+
    -- > |BYMINUTE  |Expand |
    -- > +----------+-------+
    -- > |BYSECOND  |Expand |
    -- > +----------+-------+
    -- > |BYSETPOS  |Limit  |
    -- > +----------+-------+
    specify "ByMonth limits the recurrence set" $ forAllValid $ \cur ->
      forAll genDailyRecurrence $ \rrule -> do
        let limit = addLocalTime nominalDay cur
        let withoutRule = rrule {rRuleByMonth = S.empty}
        let with = rruleOccurrencesUntil cur rrule limit
        let without = rruleOccurrencesUntil cur rrule limit
        S.size with `shouldSatisfy` (<= S.size without)
    specify "ByMonthDay limits the recurrence set" $ forAllValid $ \cur ->
      forAll genDailyRecurrence $ \rrule -> do
        let limit = addLocalTime nominalDay cur
        let withoutRule = rrule {rRuleByMonthDay = S.empty}
        let with = rruleOccurrencesUntil cur rrule limit
        let without = rruleOccurrencesUntil cur rrule limit
        S.size with `shouldSatisfy` (<= S.size without)
    specify "ByDay limits the recurrence set" $ forAllValid $ \cur ->
      forAll genDailyRecurrence $ \rrule -> do
        let limit = addLocalTime nominalDay cur
        let withoutRule = rrule {rRuleByDay = S.empty}
        let with = rruleOccurrencesUntil cur rrule limit
        let without = rruleOccurrencesUntil cur rrule limit
        S.size with `shouldSatisfy` (<= S.size without)
    specify "ByHour expands the recurrence set" $ forAllValid $ \cur ->
      forAll genDailyRecurrence $ \rrule -> do
        let limit = addLocalTime nominalDay cur
        let withoutRule = rrule {rRuleByHour = S.empty}
        let with = rruleOccurrencesUntil cur rrule limit
        let without = rruleOccurrencesUntil cur rrule limit
        S.size with `shouldSatisfy` (>= S.size without)
    specify "ByMinute expands the recurrence set" $ forAllValid $ \cur ->
      forAll genDailyRecurrence $ \rrule -> do
        let limit = addLocalTime nominalDay cur
        let withoutRule = rrule {rRuleByMinute = S.empty}
        let with = rruleOccurrencesUntil cur rrule limit
        let without = rruleOccurrencesUntil cur rrule limit
        S.size with `shouldSatisfy` (>= S.size without)
    specify "BySecond expands the recurrence set" $ forAllValid $ \cur ->
      forAll genDailyRecurrence $ \rrule -> do
        let limit = addLocalTime nominalDay cur
        let withoutRule = rrule {rRuleBySecond = S.empty}
        let with = rruleOccurrencesUntil cur rrule limit
        let without = rruleOccurrencesUntil cur rrule limit
        S.size with `shouldSatisfy` (>= S.size without)
    specify "BySetPos limits the recurrence set" $ forAllValid $ \cur ->
      forAll genDailyRecurrence $ \rrule -> do
        let limit = addLocalTime nominalDay cur
        let withoutRule = rrule {rRuleBySetPos = S.empty}
        let with = rruleOccurrencesUntil cur rrule limit
        let without = rruleOccurrencesUntil cur rrule limit
        S.size with `shouldSatisfy` (<= S.size without)
