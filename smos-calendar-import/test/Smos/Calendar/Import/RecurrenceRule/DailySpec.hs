{-# LANGUAGE OverloadedLists #-}

module Smos.Calendar.Import.RecurrenceRule.DailySpec
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
  describe "rruleOccurrencesUntil" $ do
    --  An limit in the future because we don't specify indefinite rules
    let limit = LocalTime (fromGregorian 2024 01 01) midnight
    specify "it works for this complex example" $
      let rule =
            (rRule Daily)
              { rRuleInterval = Interval 3,
                rRuleUntilCount = Count 10,
                rRuleByMonthDay = [MonthDay 10, MonthDay 20, MonthDay 30],
                rRuleByMonth = [September, October]
              }
          tod = TimeOfDay 04 30 00
          start = LocalTime (fromGregorian 2020 09 10) tod
       in rruleOccurrencesUntil start rule limit
            `shouldBe` S.fromList
              [ LocalTime (fromGregorian 2020 09 10) tod,
                LocalTime (fromGregorian 2020 10 10) tod,
                LocalTime (fromGregorian 2021 09 20) tod,
                LocalTime (fromGregorian 2021 10 20) tod,
                LocalTime (fromGregorian 2022 09 30) tod,
                LocalTime (fromGregorian 2022 10 30) tod,
                LocalTime (fromGregorian 2023 09 10) tod,
                LocalTime (fromGregorian 2023 10 10) tod
              ]
  describe "dailyNextRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = LocalTime (fromGregorian 2021 01 01) midnight
    describe "No ByX's" $ do
      specify "Every day" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 06) tod) limit (Interval 1) [] [] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 07) tod)
      specify "Every other day" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 06) tod) limit (Interval 2) [] [] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 08) tod)
    describe "ByMonth" $ do
      specify "Every three days in September" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 06) tod) limit (Interval 3) [September] [] [] [] [] [] []
          `shouldBe` Just
            (LocalTime (fromGregorian 2020 09 02) tod)
      specify "Every four days in August" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 06) tod) limit (Interval 4) [August] [] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 10) tod)
    describe "ByMonthDay" $ do
      specify "Every tenth day of the month" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 10) tod) limit (Interval 1) [] [MonthDay 10] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 09 10) tod)
      specify "Every tenth of September" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2019 09 10) tod) limit (Interval 1) [September] [MonthDay 10] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 09 10) tod)
      specify "Every last day of February in a non-leap year" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2018 02 28) tod) limit (Interval 1) [February] [MonthDay (-1)] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2019 02 28) tod)
      specify "Every last day of February in a leap year" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2019 02 28) tod) limit (Interval 1) [February] [MonthDay (-1)] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 02 29) tod)
      specify "Every second-to-last day of September" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2019 09 33) tod) limit (Interval 1) [September] [MonthDay (-1)] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 09 33) tod)
    describe "ByDay" $ do
      specify "Every tuesday" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 04) tod) limit (Interval 1) [] [] [Every Tuesday] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 11) tod)
      specify "Every tuesday in September" $ forAllValid $ \tod ->
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 04) tod) limit (Interval 1) [September] [] [Every Tuesday] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 09 01) tod)
    describe "ByHour" $ do
      specify "16h every other day" $
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 06) (TimeOfDay 16 00 00)) limit (Interval 2) [] [] [] [Hour 16] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 08) (TimeOfDay 16 00 00))
    describe "ByMinute" $ do
      specify "16h20 every third day" $
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 06) (TimeOfDay 16 20 00)) limit (Interval 3) [] [] [] [Hour 16] [Minute 20] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 09) (TimeOfDay 16 20 00))
    describe "ByMinute" $ do
      specify "16h20m30s every fourth day" $
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 06) (TimeOfDay 15 00 00)) limit (Interval 4) [] [] [] [Hour 16] [Minute 20] [Second 30] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 06) (TimeOfDay 16 20 30))
      specify "every 15th and 20th second" $
        dailyNextRecurrence (LocalTime (fromGregorian 2020 08 06) (TimeOfDay 15 00 15)) limit (Interval 1) [] [] [] [] [] [Second 15, Second 20] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 06) (TimeOfDay 15 00 20))
