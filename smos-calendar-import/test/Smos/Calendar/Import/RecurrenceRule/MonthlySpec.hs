{-# LANGUAGE OverloadedLists #-}

module Smos.Calendar.Import.RecurrenceRule.MonthlySpec
  ( spec,
  )
where

import Data.Time
import Safe
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurrenceRule.Gen ()
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Monthly
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  let d = fromGregorian
  let l = LocalTime
  let t = TimeOfDay
  describe "rruleDateTimeOccurrencesUntil" $ do
    specify "it works for this complex example" $
      forAllValid $ \tod ->
        let limit = LocalTime (d 2024 01 01) midnight
            rule =
              (rRule Monthly)
                { rRuleInterval = Interval 2,
                  rRuleUntilCount = Count 5,
                  rRuleByDay = [Every Tuesday, Every Sunday],
                  rRuleByMonth = [March, April, August],
                  rRuleWeekStart = Wednesday,
                  rRuleBySetPos = [SetPos (-1)]
                }
            start = LocalTime (d 2020 08 30) tod
         in --  This limit will be reached and cut of 2 recurrences
            rruleDateTimeOccurrencesUntil start rule limit
              `shouldBe` [ LocalTime (d 2020 08 30) tod,
                           LocalTime (d 2021 04 27) tod,
                           LocalTime (d 2021 08 31) tod,
                           LocalTime (d 2022 04 26) tod,
                           LocalTime (d 2022 08 30) tod
                         ]
  describe "monthlyDateTimeRecurrence" $ do
    let monthlyDateTimeNextRecurrence start lim i ba bb bc bd be bf bg =
          headMay $ monthlyDateTimeRecurrence start lim i ba bb bc bd be bf bg
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = l (d 2022 01 01) midnight
    describe "No ByX's" $ do
      specify "Every month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (l (d 2020 08 08) tod) limit (Interval 1) [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 09 08) tod)
      specify "Every other month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (l (d 2020 08 08) tod) limit (Interval 2) [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 10 08) tod)
    describe "ByMonth" $ do
      specify "Every month in Sept" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (l (d 2019 09 30) tod) limit (Interval 1) [September] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 09 30) tod)
      specify "Every other month in Sept" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (l (d 2019 09 30) tod) limit (Interval 2) [September] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 09 30) tod)
      specify "Every five months in Sept" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (l (d 2015 09 30) tod) limit (Interval 5) [September] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 09 30) tod)
    -- No 'ByWeekNo' because it's excluded by the table
    -- No 'ByYearDay' because it's excluded by the table
    describe "ByDay" $ do
      specify "Every wednesday and thursday" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (l (d 2020 08 12) tod) limit (Interval 1) [] [] [Every Wednesday, Every Thursday] [] [] [] []
            `shouldBe` Just (l (d 2020 08 13) tod)
      specify "Every other thursday and friday" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (l (d 2020 08 28) tod) limit (Interval 2) [] [] [Every Thursday, Every Friday] [] [] [] []
            `shouldBe` Just (l (d 2020 10 01) tod)
      specify "Every sunday, at the end of the year" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (l (d 2019 12 29) tod) limit (Interval 2) [] [] [Every Sunday] [] [] [] []
            `shouldBe` Just (l (d 2020 02 02) tod)
      specify "Every other sunday, at the end of the year" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (l (d 2019 12 29) tod) limit (Interval 2) [] [] [Every Sunday] [] [] [] []
            `shouldBe` Just (l (d 2020 02 02) tod)
      specify "Every first wednesday of the month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (l (d 2020 08 05) tod) limit (Interval 1) [] [] [Specific 1 Wednesday] [] [] [] []
            `shouldBe` Just (l (d 2020 09 02) tod)
      specify "Every other first thursday" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (l (d 2020 08 06) tod) limit (Interval 2) [] [] [Specific 1 Thursday] [] [] [] []
            `shouldBe` Just (l (d 2020 10 01) tod)
      specify "Every last wednesday of the month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (l (d 2020 08 26) tod) limit (Interval 1) [] [] [Specific (-1) Wednesday] [] [] [] []
            `shouldBe` Just (l (d 2020 09 30) tod)
      specify "Every wednesday that falls on the 10th of the month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (l (d 2019 07 10) tod) limit (Interval 1) [] [MonthDay 10] [Every Wednesday] [] [] [] []
            `shouldBe` Just (l (d 2020 06 10) tod)
      specify "Every second wednesday that falls on the 10th of the month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (l (d 2019 07 10) tod) limit (Interval 1) [] [MonthDay 10] [Specific 2 Wednesday] [] [] [] []
            `shouldBe` Just (l (d 2020 06 10) tod)
      specify "Every last wednesday that falls on the 27th of the month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (l (d 2019 03 27) tod) limit (Interval 1) [] [MonthDay 27] [Specific (-1) Wednesday] [] [] [] []
            `shouldBe` Just (l (d 2019 11 27) tod)
    describe "ByHour" $ do
      specify "16h every other month" $
        monthlyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 16 00 00)) limit (Interval 2) [] [] [] [Hour 16] [] [] []
          `shouldBe` Just (LocalTime (d 2020 10 06) (t 16 00 00))
    describe "ByMinute" $ do
      specify "16h20 every third month" $
        monthlyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 16 20 00)) limit (Interval 3) [] [] [] [Hour 16] [Minute 20] [] []
          `shouldBe` Just (LocalTime (d 2020 11 06) (t 16 20 00))
    describe "BySecond" $ do
      specify "16h20m30s every fourth month" $
        monthlyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 16 20 30)) limit (Interval 4) [] [] [] [Hour 16] [Minute 20] [Second 30] []
          `shouldBe` Just (LocalTime (d 2020 12 06) (t 16 20 30))
      specify "every 15th and 20th second" $
        monthlyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 15 00 15)) limit (Interval 1) [] [] [] [] [] [Second 15, Second 20] []
          `shouldBe` Just (LocalTime (d 2020 08 06) (t 15 00 20))
    describe "BySetPos" $ do
      specify "The last weekday of the month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextRecurrence (LocalTime (d 2020 04 30) tod) limit (Interval 1) [] [] [Every Monday, Every Tuesday, Every Wednesday, Every Thursday, Every Friday] [] [] [] [SetPos (-1)]
            `shouldBe` Just (LocalTime (d 2020 05 29) tod)
  describe "monthlyDateNextRecurrence" $ do
    let monthlyDateNextRecurrence start lim i ba bb bc bd =
          headMay $ monthlyDateRecurrence start lim i ba bb bc bd
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = d 2023 01 01
    describe "No ByX's" $ do
      specify "Every month" $
        monthlyDateNextRecurrence (d 2020 08 08) limit (Interval 1) [] [] [] []
          `shouldBe` Just (d 2020 09 08)
      specify "Every other month" $
        monthlyDateNextRecurrence (d 2020 08 08) limit (Interval 2) [] [] [] []
          `shouldBe` Just (d 2020 10 08)
    describe "ByMonth" $ do
      specify "Every month in Sept" $
        monthlyDateNextRecurrence (d 2019 09 30) limit (Interval 1) [September] [] [] []
          `shouldBe` Just (d 2020 09 30)
      specify "Every other month in Sept" $
        monthlyDateNextRecurrence (d 2019 09 30) limit (Interval 2) [September] [] [] []
          `shouldBe` Just (d 2020 09 30)
      specify "Every five months in Sept" $
        monthlyDateNextRecurrence (d 2015 09 30) limit (Interval 5) [September] [] [] []
          `shouldBe` Just (d 2020 09 30)
    -- No 'ByWeekNo' because it's excluded by the table
    -- No 'ByYearDay' because it's excluded by the table
    describe "ByDay" $ do
      specify "Every wednesday and thursday" $
        monthlyDateNextRecurrence (d 2020 08 12) limit (Interval 1) [] [] [Every Wednesday, Every Thursday] []
          `shouldBe` Just (d 2020 08 13)
      specify "Every other thursday and friday" $
        monthlyDateNextRecurrence (d 2020 08 28) limit (Interval 2) [] [] [Every Thursday, Every Friday] []
          `shouldBe` Just (d 2020 10 01)
      specify "Every sunday, at the end of the year" $
        monthlyDateNextRecurrence (d 2019 12 29) limit (Interval 2) [] [] [Every Sunday] []
          `shouldBe` Just (d 2020 02 02)
      specify "Every other month, ever sunday, at the end of the year" $
        monthlyDateNextRecurrence (d 2019 12 29) limit (Interval 2) [] [] [Every Sunday] []
          `shouldBe` Just (d 2020 02 02)
      specify "Every first wednesday of the month" $
        monthlyDateNextRecurrence (d 2020 08 05) limit (Interval 1) [] [] [Specific 1 Wednesday] []
          `shouldBe` Just (d 2020 09 02)
      specify "Every other first thursday" $
        monthlyDateNextRecurrence (d 2020 08 06) limit (Interval 2) [] [] [Specific 1 Thursday] []
          `shouldBe` Just (d 2020 10 01)
      specify "Every last wednesday of the month" $
        monthlyDateNextRecurrence (d 2020 08 26) limit (Interval 1) [] [] [Specific (-1) Wednesday] []
          `shouldBe` Just (d 2020 09 30)
      specify "Every wednesday that falls on the 10th of the month" $
        monthlyDateNextRecurrence (d 2019 07 10) limit (Interval 1) [] [MonthDay 10] [Every Wednesday] []
          `shouldBe` Just (d 2020 06 10)
      specify "Every second wednesday that falls on the 10th of the month" $
        monthlyDateNextRecurrence (d 2019 07 10) limit (Interval 1) [] [MonthDay 10] [Specific 2 Wednesday] []
          `shouldBe` Just (d 2020 06 10)
      specify "Every last wednesday that falls on the 27th of the month" $
        monthlyDateNextRecurrence (d 2019 03 27) limit (Interval 1) [] [MonthDay 27] [Specific (-1) Wednesday] []
          `shouldBe` Just (d 2019 11 27)
    describe "BySetPos" $ do
      specify "This special case" $
        monthlyDateNextRecurrence (d 2021 08 31) limit (Interval 2) [March, April, August] [] [Every Tuesday, Every Sunday] [SetPos (-1)]
          `shouldBe` Just (d 2022 04 26)
