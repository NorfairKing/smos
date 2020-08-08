{-# LANGUAGE OverloadedLists #-}

module Smos.Calendar.Import.RecurrenceRule.DailySpec
  ( spec,
  )
where

import qualified Data.Set as S
import Data.Time
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurrenceRule.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  describe "rruleDateTimeOccurrencesUntil" $ do
    specify "it works for this complex example" $
      let limit = LocalTime (fromGregorian 2024 01 01) midnight
          rule =
            (rRule Daily)
              { rRuleInterval = Interval 3,
                rRuleUntilCount = Count 10,
                rRuleByMonthDay = [MonthDay 10, MonthDay 20, MonthDay 30],
                rRuleByMonth = [September, October]
              }
          tod = TimeOfDay 04 30 00
          start = LocalTime (fromGregorian 2020 09 10) tod
       in --  This limit will be reached and cut of 2 recurrences
          rruleDateTimeOccurrencesUntil start rule limit
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
    specify "It works for this BYSETPOS example: The last hour of every day" $
      --  An limit in the future because it won't be reached anyway
      let limit = LocalTime (fromGregorian 2024 01 01) midnight
          rule =
            (rRule Daily)
              { rRuleInterval = Interval 1,
                rRuleUntilCount = Count 3,
                rRuleBySetPos = [SetPos (-1)],
                rRuleByHour =
                  [ Hour 20,
                    Hour 21,
                    Hour 22,
                    Hour 23
                  ],
                rRuleByMinute = [Minute 0],
                rRuleBySecond = [Second 0]
              }
          start = LocalTime (fromGregorian 2020 08 07) (TimeOfDay 23 00 00)
       in rruleDateTimeOccurrencesUntil start rule limit
            `shouldBe` S.fromList
              [ LocalTime (fromGregorian 2020 08 07) (TimeOfDay 23 00 00),
                LocalTime (fromGregorian 2020 08 08) (TimeOfDay 23 00 00),
                LocalTime (fromGregorian 2020 08 09) (TimeOfDay 23 00 00)
              ]
  describe "dailyDateTimeNextRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = LocalTime (fromGregorian 2021 01 01) midnight
    describe "No ByX's" $ do
      specify "Every day" $ forAllValid $ \tod ->
        dailyDateTimeNextRecurrence (LocalTime (fromGregorian 2020 08 06) tod) limit (Interval 1) [] [] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 07) tod)
      specify "Every other day" $ forAllValid $ \tod ->
        dailyDateTimeNextRecurrence (LocalTime (fromGregorian 2020 08 06) tod) limit (Interval 2) [] [] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 08) tod)
    describe "ByMonth" $ do
      specify "Every three days in September" $ forAllValid $ \tod ->
        dailyDateTimeNextRecurrence (LocalTime (fromGregorian 2020 08 06) tod) limit (Interval 3) [September] [] [] [] [] [] []
          `shouldBe` Just
            (LocalTime (fromGregorian 2020 09 02) tod)
      specify "Every four days in August" $ forAllValid $ \tod ->
        dailyDateTimeNextRecurrence (LocalTime (fromGregorian 2020 08 06) tod) limit (Interval 4) [August] [] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 10) tod)
    describe "ByMonthDay" $ do
      specify "Every tenth day of the month" $ forAllValid $ \tod ->
        dailyDateTimeNextRecurrence (LocalTime (fromGregorian 2020 08 10) tod) limit (Interval 1) [] [MonthDay 10] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 09 10) tod)
      specify "Every tenth of September" $ forAllValid $ \tod ->
        dailyDateTimeNextRecurrence (LocalTime (fromGregorian 2019 09 10) tod) limit (Interval 1) [September] [MonthDay 10] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 09 10) tod)
      specify "Every last day of February in a non-leap year" $ forAllValid $ \tod ->
        dailyDateTimeNextRecurrence (LocalTime (fromGregorian 2018 02 28) tod) limit (Interval 1) [February] [MonthDay (-1)] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2019 02 28) tod)
      specify "Every last day of February in a leap year" $ forAllValid $ \tod ->
        dailyDateTimeNextRecurrence (LocalTime (fromGregorian 2019 02 28) tod) limit (Interval 1) [February] [MonthDay (-1)] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 02 29) tod)
      specify "Every second-to-last day of September" $ forAllValid $ \tod ->
        dailyDateTimeNextRecurrence (LocalTime (fromGregorian 2019 09 33) tod) limit (Interval 1) [September] [MonthDay (-1)] [] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 09 33) tod)
    describe "ByDay" $ do
      specify "Every tuesday" $ forAllValid $ \tod ->
        dailyDateTimeNextRecurrence (LocalTime (fromGregorian 2020 08 04) tod) limit (Interval 1) [] [] [Every Tuesday] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 11) tod)
      specify "Every tuesday in September" $ forAllValid $ \tod ->
        dailyDateTimeNextRecurrence (LocalTime (fromGregorian 2020 08 04) tod) limit (Interval 1) [September] [] [Every Tuesday] [] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 09 01) tod)
    describe "ByHour" $ do
      specify "16h every other day" $
        dailyDateTimeNextRecurrence (LocalTime (fromGregorian 2020 08 06) (TimeOfDay 16 00 00)) limit (Interval 2) [] [] [] [Hour 16] [] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 08) (TimeOfDay 16 00 00))
    describe "ByMinute" $ do
      specify "16h20 every third day" $
        dailyDateTimeNextRecurrence (LocalTime (fromGregorian 2020 08 06) (TimeOfDay 16 20 00)) limit (Interval 3) [] [] [] [Hour 16] [Minute 20] [] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 09) (TimeOfDay 16 20 00))
    describe "ByMinute" $ do
      specify "16h20m30s every fourth day" $
        dailyDateTimeNextRecurrence (LocalTime (fromGregorian 2020 08 06) (TimeOfDay 15 00 00)) limit (Interval 4) [] [] [] [Hour 16] [Minute 20] [Second 30] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 06) (TimeOfDay 16 20 30))
      specify "every 15th and 20th second" $
        dailyDateTimeNextRecurrence (LocalTime (fromGregorian 2020 08 06) (TimeOfDay 15 00 15)) limit (Interval 1) [] [] [] [] [] [Second 15, Second 20] []
          `shouldBe` Just (LocalTime (fromGregorian 2020 08 06) (TimeOfDay 15 00 20))
  describe "dailyDateNextRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = fromGregorian 2021 01 01
    describe "No ByX's" $ do
      specify "Every day" $
        dailyDateNextRecurrence (fromGregorian 2020 08 06) limit (Interval 1) [] [] [] []
          `shouldBe` Just (fromGregorian 2020 08 07)
      specify "Every other day" $
        dailyDateNextRecurrence (fromGregorian 2020 08 06) limit (Interval 2) [] [] [] []
          `shouldBe` Just (fromGregorian 2020 08 08)
    describe "ByMonth" $ do
      specify "Every three days in September" $
        dailyDateNextRecurrence (fromGregorian 2020 08 06) limit (Interval 3) [September] [] [] []
          `shouldBe` Just (fromGregorian 2020 09 02)
      specify "Every four days in August" $
        dailyDateNextRecurrence (fromGregorian 2020 08 06) limit (Interval 4) [August] [] [] []
          `shouldBe` Just (fromGregorian 2020 08 10)
    describe "ByMonthDay" $ do
      specify "Every tenth day of the month" $
        dailyDateNextRecurrence (fromGregorian 2020 08 10) limit (Interval 1) [] [MonthDay 10] [] []
          `shouldBe` Just (fromGregorian 2020 09 10)
      specify "Every tenth of September" $
        dailyDateNextRecurrence (fromGregorian 2019 09 10) limit (Interval 1) [September] [MonthDay 10] [] []
          `shouldBe` Just (fromGregorian 2020 09 10)
      specify "Every last day of February in a non-leap year" $
        dailyDateNextRecurrence (fromGregorian 2018 02 28) limit (Interval 1) [February] [MonthDay (-1)] [] []
          `shouldBe` Just (fromGregorian 2019 02 28)
      specify "Every last day of February in a leap year" $
        dailyDateNextRecurrence (fromGregorian 2019 02 28) limit (Interval 1) [February] [MonthDay (-1)] [] []
          `shouldBe` Just (fromGregorian 2020 02 29)
      specify "Every second-to-last day of September" $
        dailyDateNextRecurrence (fromGregorian 2019 09 33) limit (Interval 1) [September] [MonthDay (-1)] [] []
          `shouldBe` Just (fromGregorian 2020 09 33)
    describe "ByDay" $ do
      specify "Every tuesday" $
        dailyDateNextRecurrence (fromGregorian 2020 08 04) limit (Interval 1) [] [] [Every Tuesday] []
          `shouldBe` Just (fromGregorian 2020 08 11)
      specify "Every tuesday in September" $
        dailyDateNextRecurrence (fromGregorian 2020 08 04) limit (Interval 1) [September] [] [Every Tuesday] []
          `shouldBe` Just (fromGregorian 2020 09 01)
