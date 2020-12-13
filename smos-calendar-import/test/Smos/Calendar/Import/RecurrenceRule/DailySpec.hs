{-# LANGUAGE OverloadedLists #-}

module Smos.Calendar.Import.RecurrenceRule.DailySpec
  ( spec,
  )
where

import Data.Time
import Safe
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurrenceRule.Gen ()
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Daily
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  let d = fromGregorian
      t = TimeOfDay
  describe "rruleDateTimeOccurrencesUntil" $ do
    specify "it works for this complex example" $
      let limit = LocalTime (d 2024 01 01) midnight
          rule =
            (rRule Daily)
              { rRuleInterval = Interval 3,
                rRuleUntilCount = Count 10,
                rRuleByMonthDay = [MonthDay 10, MonthDay 20, MonthDay 30],
                rRuleByMonth = [September, October]
              }
          tod = t 04 30 00
          start = LocalTime (d 2020 09 10) tod
       in --  This limit will be reached and cut of 2 recurrences
          rruleDateTimeOccurrencesUntil start rule limit
            `shouldBe` [ LocalTime (d 2020 09 10) tod,
                         LocalTime (d 2020 10 10) tod,
                         LocalTime (d 2021 09 20) tod,
                         LocalTime (d 2021 10 20) tod,
                         LocalTime (d 2022 09 30) tod,
                         LocalTime (d 2022 10 30) tod,
                         LocalTime (d 2023 09 10) tod,
                         LocalTime (d 2023 10 10) tod
                       ]
    specify "It works for this BYSETPOS example: The last hour of every day" $
      --  An limit in the future because it won't be reached anyway
      let limit = LocalTime (d 2024 01 01) midnight
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
          start = LocalTime (d 2020 08 07) (t 23 00 00)
       in rruleDateTimeOccurrencesUntil start rule limit
            `shouldBe` [ LocalTime (d 2020 08 07) (t 23 00 00),
                         LocalTime (d 2020 08 08) (t 23 00 00),
                         LocalTime (d 2020 08 09) (t 23 00 00)
                       ]
    specify "It works for this BYSETPOS example: The last two hours of every day" $
      --  An limit in the future because it won't be reached anyway
      let limit = LocalTime (d 2024 01 01) midnight
          rule =
            (rRule Daily)
              { rRuleInterval = Interval 1,
                rRuleUntilCount = Count 4,
                rRuleBySetPos = [SetPos (-1), SetPos (-2)],
                rRuleByHour =
                  [ Hour 22,
                    Hour 23
                  ],
                rRuleByMinute = [Minute 0],
                rRuleBySecond = [Second 0]
              }
          start = LocalTime (d 2020 08 07) (t 23 00 00)
       in rruleDateTimeOccurrencesUntil start rule limit
            `shouldBe` [ LocalTime (d 2020 08 07) (t 23 00 00),
                         LocalTime (d 2020 08 08) (t 22 00 00),
                         LocalTime (d 2020 08 08) (t 23 00 00),
                         LocalTime (d 2020 08 09) (t 22 00 00)
                       ]
  describe "dailyDateTimeRecurrence" $ do
    let dailyDateTimeNextRecurrence start lim i ba bb bc bd be bf bg =
          headMay $ dailyDateTimeRecurrence start lim i ba bb bc bd be bf bg
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = LocalTime (d 2021 01 01) midnight
    describe "No ByX's" $ do
      specify "Every day" $
        forAllValid $ \tod ->
          dailyDateTimeNextRecurrence (LocalTime (d 2020 08 06) tod) limit (Interval 1) [] [] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 08 07) tod)
      specify "Every other day" $
        forAllValid $ \tod ->
          dailyDateTimeNextRecurrence (LocalTime (d 2020 08 06) tod) limit (Interval 2) [] [] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 08 08) tod)
    describe "ByMonth" $ do
      specify "Every three days in September" $
        forAllValid $ \tod ->
          dailyDateTimeNextRecurrence (LocalTime (d 2020 08 06) tod) limit (Interval 3) [September] [] [] [] [] [] []
            `shouldBe` Just
              (LocalTime (d 2020 09 02) tod)
      specify "Every four days in August" $
        forAllValid $ \tod ->
          dailyDateTimeNextRecurrence (LocalTime (d 2020 08 06) tod) limit (Interval 4) [August] [] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 08 10) tod)
    describe "ByMonthDay" $ do
      specify "Every tenth day of the month" $
        forAllValid $ \tod ->
          dailyDateTimeNextRecurrence (LocalTime (d 2020 08 10) tod) limit (Interval 1) [] [MonthDay 10] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 09 10) tod)
      specify "Every tenth of September" $
        forAllValid $ \tod ->
          dailyDateTimeNextRecurrence (LocalTime (d 2019 09 10) tod) limit (Interval 1) [September] [MonthDay 10] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 09 10) tod)
      specify "Every last day of February in a non-leap year" $
        forAllValid $ \tod ->
          dailyDateTimeNextRecurrence (LocalTime (d 2018 02 28) tod) limit (Interval 1) [February] [MonthDay (-1)] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2019 02 28) tod)
      specify "Every last day of February in a leap year" $
        forAllValid $ \tod ->
          dailyDateTimeNextRecurrence (LocalTime (d 2019 02 28) tod) limit (Interval 1) [February] [MonthDay (-1)] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 02 29) tod)
      specify "Every second-to-last day of September" $
        forAllValid $ \tod ->
          dailyDateTimeNextRecurrence (LocalTime (d 2019 09 33) tod) limit (Interval 1) [September] [MonthDay (-1)] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 09 33) tod)
    describe "ByDay" $ do
      specify "Every tuesday" $
        forAllValid $ \tod ->
          dailyDateTimeNextRecurrence (LocalTime (d 2020 08 04) tod) limit (Interval 1) [] [] [Tuesday] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 08 11) tod)
      specify "Every tuesday in September" $
        forAllValid $ \tod ->
          dailyDateTimeNextRecurrence (LocalTime (d 2020 08 04) tod) limit (Interval 1) [September] [] [Tuesday] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 09 01) tod)
    describe "ByHour" $ do
      specify "16h every other day" $
        dailyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 16 00 00)) limit (Interval 2) [] [] [] [Hour 16] [] [] []
          `shouldBe` Just (LocalTime (d 2020 08 08) (t 16 00 00))
    describe "ByMinute" $ do
      specify "16h20 every third day" $
        dailyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 16 20 00)) limit (Interval 3) [] [] [] [Hour 16] [Minute 20] [] []
          `shouldBe` Just (LocalTime (d 2020 08 09) (t 16 20 00))
    describe "BySecond" $ do
      specify "16h20m30s every fourth day" $
        dailyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 15 00 00)) limit (Interval 4) [] [] [] [Hour 16] [Minute 20] [Second 30] []
          `shouldBe` Just (LocalTime (d 2020 08 06) (t 16 20 30))
      specify "every 15th and 20th second" $
        dailyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 15 00 15)) limit (Interval 1) [] [] [] [] [] [Second 15, Second 20] []
          `shouldBe` Just (LocalTime (d 2020 08 06) (t 15 00 20))
  describe "rruleDateOccurrencesUntil" $ do
    specify "it works for this complex example" $
      let limit = d 2024 01 01
          rule =
            (rRule Daily)
              { rRuleInterval = Interval 3,
                rRuleUntilCount = Count 10,
                rRuleByMonthDay = [MonthDay 10, MonthDay 20, MonthDay 30],
                rRuleByMonth = [September, October]
              }
          start = d 2020 09 10
       in --  This limit will be reached and cut of 2 recurrences
          rruleDateOccurrencesUntil start rule limit
            `shouldBe` [ d 2020 09 10,
                         d 2020 10 10,
                         d 2021 09 20,
                         d 2021 10 20,
                         d 2022 09 30,
                         d 2022 10 30,
                         d 2023 09 10,
                         d 2023 10 10
                       ]
    specify "It works for this BYSETPOS example: The last hour of every day (this is the same as just 'every day'.)" $
      --  An limit in the future because it won't be reached anyway
      let limit = d 2024 01 01
          rule =
            (rRule Daily)
              { rRuleInterval = Interval 1,
                rRuleUntilCount = Count 3,
                rRuleBySetPos = [SetPos (-1)]
              }
          start = d 2020 08 07
       in rruleDateOccurrencesUntil start rule limit
            `shouldBe` [ d 2020 08 07,
                         d 2020 08 08,
                         d 2020 08 09
                       ]
  describe "dailyDateNextRecurrence" $ do
    let dailyDateNextRecurrence start lim i ba bb bc =
          headMay $ dailyDateRecurrence start lim i ba bb bc
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = d 2021 01 01
    describe "No ByX's" $ do
      specify "Every day" $
        dailyDateNextRecurrence (d 2020 08 06) limit (Interval 1) [] [] []
          `shouldBe` Just (d 2020 08 07)
      specify "Every other day" $
        dailyDateNextRecurrence (d 2020 08 06) limit (Interval 2) [] [] []
          `shouldBe` Just (d 2020 08 08)
    describe "ByMonth" $ do
      specify "Every three days in September" $
        dailyDateNextRecurrence (d 2020 08 06) limit (Interval 3) [September] [] []
          `shouldBe` Just (d 2020 09 02)
      specify "Every four days in August" $
        dailyDateNextRecurrence (d 2020 08 06) limit (Interval 4) [August] [] []
          `shouldBe` Just (d 2020 08 10)
    describe "ByMonthDay" $ do
      specify "Every tenth day of the month" $
        dailyDateNextRecurrence (d 2020 08 10) limit (Interval 1) [] [MonthDay 10] []
          `shouldBe` Just (d 2020 09 10)
      specify "Every tenth of September" $
        dailyDateNextRecurrence (d 2019 09 10) limit (Interval 1) [September] [MonthDay 10] []
          `shouldBe` Just (d 2020 09 10)
      specify "Every last day of February in a non-leap year" $
        dailyDateNextRecurrence (d 2018 02 28) limit (Interval 1) [February] [MonthDay (-1)] []
          `shouldBe` Just (d 2019 02 28)
      specify "Every last day of February in a leap year" $
        dailyDateNextRecurrence (d 2019 02 28) limit (Interval 1) [February] [MonthDay (-1)] []
          `shouldBe` Just (d 2020 02 29)
      specify "Every second-to-last day of September" $
        dailyDateNextRecurrence (d 2019 09 33) limit (Interval 1) [September] [MonthDay (-1)] []
          `shouldBe` Just (d 2020 09 33)
    describe "ByDay" $ do
      specify "Every tuesday" $
        dailyDateNextRecurrence (d 2020 08 04) limit (Interval 1) [] [] [Tuesday]
          `shouldBe` Just (d 2020 08 11)
      specify "Every tuesday in September" $
        dailyDateNextRecurrence (d 2020 08 04) limit (Interval 1) [September] [] [Tuesday]
          `shouldBe` Just (d 2020 09 01)
