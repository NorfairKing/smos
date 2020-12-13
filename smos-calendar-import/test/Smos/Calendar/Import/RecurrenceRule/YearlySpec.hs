{-# LANGUAGE OverloadedLists #-}

module Smos.Calendar.Import.RecurrenceRule.YearlySpec
  ( spec,
  )
where

import Data.Time
import Safe
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurrenceRule.Gen ()
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Yearly
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  let d = fromGregorian
  let l = LocalTime
  let yearlyDateTimeNextRecurrence start lim i ba bb bc bd be bf bg bh bi bj =
        headMay $ yearlyDateTimeRecurrence start lim i ba bb bc bd be bf bg bh bi bj
  let yearlyDateNextRecurrence start lim i ba bb bc bd be bf bg =
        headMay $ yearlyDateRecurrence start lim i ba bb bc bd be bf bg
  describe "yearlyDateTimeRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrences
    let limit = l (d 2030 01 01) midnight
    describe "No ByX's" $ do
      specify "Every year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2020 08 08) tod) limit (Interval 1) [] Monday [] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2021 08 08) tod)
      specify "Every other year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2020 08 08) tod) limit (Interval 2) [] Monday [] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2022 08 08) tod)
    describe "ByMonth" $ do
      specify "Every year in Sept" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2019 09 30) tod) limit (Interval 1) [September] Monday [] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 09 30) tod)
      specify "Every other year in Sept" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2019 10 30) tod) limit (Interval 2) [October] Monday [] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2021 10 30) tod)
      specify "Every five years in Sept" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2015 11 30) tod) limit (Interval 5) [November] Monday [] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 11 30) tod)
      specify "Every year in Sept and Nov" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2019 09 30) tod) limit (Interval 1) [September, November] Monday [] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2019 11 30) tod)
    describe "ByWeekNo" $ do
      specify "Every last week of the year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2019 12 31) tod) limit (Interval 1) [] Monday [WeekNo (-1)] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 12 28) tod)
      specify "Every sixth week in february" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2025 02 09) tod) limit (Interval 1) [February] Monday [WeekNo 6] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2026 02 02) tod)
    describe "ByYearDay" $ do
      specify "Every first and last day of the year, at the end" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2019 12 31) tod) limit (Interval 1) [] Monday [] [YearDay 1, YearDay (-1)] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 01 01) tod)
      specify "Every first and last day of the year, at the start" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2019 01 01) tod) limit (Interval 1) [] Monday [] [YearDay 1, YearDay (-1)] [] [] [] [] [] []
            `shouldBe` Just (l (d 2019 12 31) tod)
      specify "Every February" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2019 02 05) tod) limit (Interval 1) [February] Monday [] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 02 05) tod)
      specify "Every first day of the year, as long as it's also in the first week of the year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2026 01 01) tod) limit (Interval 1) [] Monday [WeekNo 1] [YearDay 1] [] [] [] [] [] []
            `shouldBe` Just (l (d 2029 01 01) tod)
      specify "Every 1st of march, except on leap years" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2019 03 01) tod) limit (Interval 1) [March] Monday [] [YearDay 60] [] [] [] [] [] []
            `shouldBe` Just (l (d 2021 03 01) tod)
    describe "ByMonthDay" $ do
      specify "Every 29th day of every month" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2019 01 29) tod) limit (Interval 1) [] Monday [] [] [MonthDay 29] [] [] [] [] []
            `shouldBe` Just (l (d 2019 03 29) tod)
      specify "Every 15th and 20th day of every month" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2020 01 20) tod) limit (Interval 1) [] Monday [] [] [MonthDay 15, MonthDay 20] [] [] [] [] []
            `shouldBe` Just (l (d 2020 02 15) tod)
      specify "Every 15th and 20th day of every February and March" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2020 03 20) tod) limit (Interval 1) [February, March] Monday [] [] [MonthDay 15, MonthDay 20] [] [] [] [] []
            `shouldBe` Just (l (d 2021 02 15) tod)
      specify "Every 29th day of the month that is also the 60th day of the year (29 feb)" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2020 02 29) tod) limit (Interval 1) [] Monday [] [YearDay 60] [MonthDay 29] [] [] [] [] []
            `shouldBe` Just (l (d 2024 02 29) tod)
      specify "Every 30th or 31st day of the month that is also the first week of the year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2018 12 31) tod) limit (Interval 1) [] Monday [WeekNo 1] [] [MonthDay 30, MonthDay 31] [] [] [] [] []
            `shouldBe` Just (l (d 2019 12 30) tod)
    describe "ByDay" $ do
      specify "Every monday and wednesday" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2020 08 12) tod) limit (Interval 1) [] Monday [] [] [] [Every Monday, Every Wednesday] [] [] [] []
            `shouldBe` Just (l (d 2020 08 17) tod)
      specify "Every first monday of the year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2020 01 06) tod) limit (Interval 1) [] Monday [] [] [] [Specific 1 Monday] [] [] [] []
            `shouldBe` Just (l (d 2021 01 04) tod)
      specify "Every monday in the first and second weeks of the year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2020 01 06) tod) limit (Interval 1) [] Monday [WeekNo 1, WeekNo 2] [] [] [Every Monday] [] [] [] []
            `shouldBe` Just (l (d 2021 01 04) tod)
      specify "Every saturday in june" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2020 06 13) tod) limit (Interval 1) [June] Monday [] [] [] [Every Saturday] [] [] [] []
            `shouldBe` Just (l (d 2020 06 20) tod)
      specify "Every fourth saturday in june" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2019 06 22) tod) limit (Interval 1) [June] Monday [] [] [] [Specific 4 Saturday] [] [] [] []
            `shouldBe` Just (l (d 2020 06 27) tod)
      specify "Every last saturday in june" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2020 06 13) tod) limit (Interval 1) [June] Monday [] [] [] [Specific (-1) Saturday] [] [] [] []
            `shouldBe` Just (l (d 2020 06 27) tod)
      specify "Every monday, the first of the month" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2021 03 01) tod) limit (Interval 1) [] Monday [] [] [MonthDay 1] [Every Monday] [] [] [] []
            `shouldBe` Just (l (d 2021 11 01) tod)
      specify "Every tuesday, on a year day divisible by 100" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2020 10 26) tod) limit (Interval 1) [] Monday [] [YearDay 100, YearDay 200, YearDay 300] [] [Every Tuesday] [] [] [] []
            `shouldBe` Just (l (d 2022 07 19) tod)
      specify "Every Monday and Tuesday in the first week of every year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextRecurrence (l (d 2019 12 31) tod) limit (Interval 1) [] Monday [WeekNo 1] [] [] [Every Monday, Every Tuesday] [] [] [] []
            `shouldBe` Just (l (d 2021 01 04) tod)
  --   describe "ByHour" $ do
  --     specify "16h every other year" $
  --       yearlyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 16 00 00)) limit (Interval 2) [] [] [] [Hour 16] [] [] [] [] [] []
  --         `shouldBe` Just (LocalTime (d 2020 10 06) (t 16 00 00))
  --   describe "ByMinute" $ do
  --     specify "16h20 every third year" $
  --       yearlyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 16 20 00)) limit (Interval 3) [] [] [] [Hour 16] [Minute 20] [] [] [] [] []
  --         `shouldBe` Just (LocalTime (d 2020 11 06) (t 16 20 00))
  --   describe "BySecond" $ do
  --     specify "16h20m30s every fourth year" $
  --       yearlyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 16 20 30)) limit (Interval 4) [] [] [] [Hour 16] [Minute 20] [Second 30] [] [] [] []
  --         `shouldBe` Just (LocalTime (d 2020 12 06) (t 16 20 30))
  --     specify "every 15th and 20th second" $
  --       yearlyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 15 00 15)) limit (Interval 1) [] [] [] [] [] [Second 15, Second 20] [] [] [] []
  --         `shouldBe` Just (LocalTime (d 2020 08 06) (t 15 00 20))
  --   describe "BySetPos" $ do
  --     specify "The last weekday of the year" $ forAllValid $ \tod ->
  --       yearlyDateTimeNextRecurrence (LocalTime (d 2020 04 30) tod) limit (Interval 1) [] [] [Every Monday, Every Tuesday, Every Wednesday, Every Thursday, Every Friday] [] [] [] [SetPos (-1)] [] [] []
  --         `shouldBe` Just (LocalTime (d 2020 05 29) tod)
  describe "yearlyDateNextRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrences
    let limit = d 2030 01 01
    describe "No ByX's" $ do
      specify "Every year" $
        yearlyDateNextRecurrence (d 2020 08 08) limit (Interval 1) [] Monday [] [] [] [] []
          `shouldBe` Just (d 2021 08 08)
      specify "Every other year" $
        yearlyDateNextRecurrence (d 2020 08 08) limit (Interval 2) [] Monday [] [] [] [] []
          `shouldBe` Just (d 2022 08 08)
    describe "ByMonth" $ do
      specify "Every year in Sept" $
        yearlyDateNextRecurrence (d 2019 09 30) limit (Interval 1) [September] Monday [] [] [] [] []
          `shouldBe` Just (d 2020 09 30)
      specify "Every other year in Sept" $
        yearlyDateNextRecurrence (d 2019 09 30) limit (Interval 2) [September] Monday [] [] [] [] []
          `shouldBe` Just (d 2021 09 30)
      specify "Every five years in Sept" $
        yearlyDateNextRecurrence (d 2015 09 30) limit (Interval 5) [September] Monday [] [] [] [] []
          `shouldBe` Just (d 2020 09 30)
      specify "Every year in Sept and Nov" $
        yearlyDateNextRecurrence (d 2019 09 30) limit (Interval 1) [September, November] Monday [] [] [] [] []
          `shouldBe` Just (d 2019 11 30)
    describe "ByWeekNo" $ do
      specify "Every last week of the year" $
        yearlyDateNextRecurrence (d 2019 12 31) limit (Interval 1) [] Monday [WeekNo (-1)] [] [] [] []
          `shouldBe` Just (d 2020 12 28)
      specify "Every sixth week, in february" $
        yearlyDateNextRecurrence (d 2025 02 09) limit (Interval 1) [February] Monday [WeekNo 6] [] [] [] []
          `shouldBe` Just (d 2026 02 02)
      specify "Every first week of the year" $
        yearlyDateNextRecurrence (d 2019 12 31) limit (Interval 1) [] Monday [WeekNo 1] [] [] [] []
          `shouldBe` Just (d 2020 01 01)
    describe "ByYearDay" $ do
      specify "Every first and last day of the year, at the end" $
        yearlyDateNextRecurrence (d 2019 12 31) limit (Interval 1) [] Monday [] [YearDay 1, YearDay (-1)] [] [] []
          `shouldBe` Just (d 2020 01 01)
      specify "Every first and last day of the year, at the start" $
        yearlyDateNextRecurrence (d 2019 01 01) limit (Interval 1) [] Monday [] [YearDay 1, YearDay (-1)] [] [] []
          `shouldBe` Just (d 2019 12 31)
      specify "Every February" $
        yearlyDateNextRecurrence (d 2019 02 05) limit (Interval 1) [February] Monday [] [] [] [] []
          `shouldBe` Just (d 2020 02 05)
      specify "Every first day of the year, as long as it's also in the first week of the year" $
        yearlyDateNextRecurrence (d 2026 01 01) limit (Interval 1) [] Monday [WeekNo 1] [YearDay 1] [] [] []
          `shouldBe` Just (d 2029 01 01)
      specify "Every 1st of march, except on leap years" $
        yearlyDateNextRecurrence (d 2019 03 01) limit (Interval 1) [March] Monday [] [YearDay 60] [] [] []
          `shouldBe` Just (d 2021 03 01)
    describe "ByMonthDay" $ do
      specify "Every 29th day of every month" $
        yearlyDateNextRecurrence (d 2019 01 29) limit (Interval 1) [] Monday [] [] [MonthDay 29] [] []
          `shouldBe` Just (d 2019 03 29)
      specify "Every 15th and 20th day of every month" $
        yearlyDateNextRecurrence (d 2020 01 20) limit (Interval 1) [] Monday [] [] [MonthDay 15, MonthDay 20] [] []
          `shouldBe` Just (d 2020 02 15)
      specify "Every 15th and 20th day of every February and March" $
        yearlyDateNextRecurrence (d 2020 03 20) limit (Interval 1) [February, March] Monday [] [] [MonthDay 15, MonthDay 20] [] []
          `shouldBe` Just (d 2021 02 15)
      specify "Every 29th day of the month that is also the 60th day of the year (29 feb)" $
        yearlyDateNextRecurrence (d 2020 02 29) limit (Interval 1) [] Monday [] [YearDay 60] [MonthDay 29] [] []
          `shouldBe` Just (d 2024 02 29)
      specify "Every 30th or 31st day of the month that is also the first week of the year" $
        yearlyDateNextRecurrence (d 2018 12 31) limit (Interval 1) [] Monday [WeekNo 1] [] [MonthDay 30, MonthDay 31] [] []
          `shouldBe` Just (d 2019 12 30)
    describe "ByDay" $ do
      specify "Every monday and wednesday" $
        yearlyDateNextRecurrence (d 2020 08 12) limit (Interval 1) [] Monday [] [] [] [Every Monday, Every Wednesday] []
          `shouldBe` Just (d 2020 08 17)
      specify "Every first monday of the year" $
        yearlyDateNextRecurrence (d 2020 01 06) limit (Interval 1) [] Monday [] [] [] [Specific 1 Monday] []
          `shouldBe` Just (d 2021 01 04)
      specify "Every 4th monday of the year" $
        yearlyDateNextRecurrence (d 2020 01 27) limit (Interval 1) [] Monday [] [] [] [Specific 4 Monday] []
          `shouldBe` Just (d 2021 01 25)
      specify "Every monday in the first and second weeks of the year" $
        yearlyDateNextRecurrence (d 2020 01 06) limit (Interval 1) [] Monday [WeekNo 1, WeekNo 2] [] [] [Every Monday] []
          `shouldBe` Just (d 2021 01 04)
      specify "Every saturday in june" $
        yearlyDateNextRecurrence (d 2020 06 13) limit (Interval 1) [June] Monday [] [] [] [Every Saturday] []
          `shouldBe` Just (d 2020 06 20)
      specify "Every fourth saturday in june" $
        yearlyDateNextRecurrence (d 2019 06 22) limit (Interval 1) [June] Monday [] [] [] [Specific 4 Saturday] []
          `shouldBe` Just (d 2020 06 27)
      specify "Every last saturday in june" $
        yearlyDateNextRecurrence (d 2020 06 13) limit (Interval 1) [June] Monday [] [] [] [Specific (-1) Saturday] []
          `shouldBe` Just (d 2020 06 27)
      specify "Every monday, the first of the month" $
        yearlyDateNextRecurrence (d 2021 03 01) limit (Interval 1) [] Monday [] [] [MonthDay 1] [Every Monday] []
          `shouldBe` Just (d 2021 11 01)
      specify "Every first monday of the year that is also the first of the month" $
        yearlyDateNextRecurrence (d 2001 01 01) limit (Interval 1) [] Monday [] [] [MonthDay 1] [Specific 1 Monday] []
          `shouldBe` Just (d 2007 01 01)
      specify "Every tuesday, on a year day divisible by 100" $
        yearlyDateNextRecurrence (d 2020 10 26) limit (Interval 1) [] Monday [] [YearDay 100, YearDay 200, YearDay 300] [] [Every Tuesday] []
          `shouldBe` Just (d 2022 07 19)
      specify "Every Monday and Tuesday in the first week of every year" $
        yearlyDateNextRecurrence (d 2019 12 31) limit (Interval 1) [] Monday [WeekNo 1] [] [] [Every Monday, Every Tuesday] []
          `shouldBe` Just (d 2021 01 04)

--
--   describe "BySetPos" $ do
--     specify "This special case" $
--       yearlyDateNextRecurrence (d 2021 08 31) limit (Interval 2) [March, April, August] [] [Every Tuesday, Every Sunday] [SetPos (-1)] [] [] []
--         `shouldBe` Just (d 2022 04 26)
