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
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  let d = fromGregorian
  let l = LocalTime
  let t = TimeOfDay
  let yearlyDateTimeNextRecurrence start lim i ba bb bc bd be bf bg bh bi bj =
        headMay $ yearlyDateTimeRecurrence start lim i ba bb bc bd be bf bg bh bi bj
  let yearlyDateNextRecurrence start lim i ba bb bc bd be bf bg =
        headMay $ yearlyDateRecurrence start lim i ba bb bc bd be bf bg
  describe "rruleDateTimeOccurrencesUntil" $ do
    specify "it works for this example weekno example" $ forAllValid $ \tod ->
      let limit = LocalTime (d 2024 01 01) midnight
          rule =
            (rRule Yearly)
              { rRuleInterval = Interval 1,
                rRuleUntilCount = Count 6,
                rRuleByWeekNo = [WeekNo 1],
                rRuleWeekStart = Wednesday,
                rRuleBySetPos = [SetPos (-1)]
              }
          start = LocalTime (d 2021 01 08) tod
       in --  This limit will be reached and cut of 2 recurrences
          rruleDateTimeOccurrencesUntil start rule limit
            `shouldBe` [ LocalTime (d 2021 01 08) tod,
                         LocalTime (d 2021 01 09) tod,
                         LocalTime (d 2021 01 10) tod,
                         LocalTime (d 2022 01 03) tod,
                         LocalTime (d 2022 01 04) tod,
                         LocalTime (d 2022 01 05) tod
                       ]
  --     let limit = LocalTime (d 2024 01 01) midnight
  --         rule =
  --           (rRule Yearly)
  --             { rRuleInterval = Interval 2,
  --               rRuleUntilCount = Count 5,
  --               rRuleByDay = [Every Tuesday, Every Sunday],
  --               rRuleByMonth = [March, April, August],
  --               rRuleWeekStart = Wednesday,
  --               rRuleBySetPos = [SetPos (-1)]
  --             }
  --         start = LocalTime (d 2020 08 30) tod
  --      in --  This limit will be reached and cut of 2 recurrences
  --         rruleDateTimeOccurrencesUntil start rule limit
  --           `shouldBe` [ LocalTime (d 2020 08 30) tod,
  --                        LocalTime (d 2021 04 27) tod,
  --                        LocalTime (d 2021 08 31) tod,
  --                        LocalTime (d 2022 04 26) tod,
  --                        LocalTime (d 2022 08 30) tod
  --                      ]
  describe "yearlyDateTimeRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrences
    let limit = l (d 2028 01 01) midnight
    describe "No ByX's" $ do
      specify "Every year" $ forAllValid $ \tod ->
        yearlyDateTimeNextRecurrence (l (d 2020 08 08) tod) limit (Interval 1) [] Monday [] [] [] [] [] [] [] []
          `shouldBe` Just (l (d 2021 08 08) tod)
      specify "Every other year" $ forAllValid $ \tod ->
        yearlyDateTimeNextRecurrence (l (d 2020 08 08) tod) limit (Interval 2) [] Monday [] [] [] [] [] [] [] []
          `shouldBe` Just (l (d 2022 08 08) tod)
    describe "ByMonth" $ do
      specify "Every year in Sept" $ forAllValid $ \tod ->
        yearlyDateTimeNextRecurrence (l (d 2019 09 30) tod) limit (Interval 1) [September] Monday [] [] [] [] [] [] [] []
          `shouldBe` Just (l (d 2020 09 30) tod)
      specify "Every other year in Sept" $ forAllValid $ \tod ->
        yearlyDateTimeNextRecurrence (l (d 2019 10 30) tod) limit (Interval 2) [October] Monday [] [] [] [] [] [] [] []
          `shouldBe` Just (l (d 2021 10 30) tod)
      specify "Every five years in Sept" $ forAllValid $ \tod ->
        yearlyDateTimeNextRecurrence (l (d 2015 11 30) tod) limit (Interval 5) [November] Monday [] [] [] [] [] [] [] []
          `shouldBe` Just (l (d 2020 11 30) tod)
      specify "Every year in Sept and Nov" $ forAllValid $ \tod ->
        yearlyDateTimeNextRecurrence (l (d 2019 09 30) tod) limit (Interval 1) [September, November] Monday [] [] [] [] [] [] [] []
          `shouldBe` Just (l (d 2019 11 30) tod)
    describe "ByWeekNo" $ do
      specify "Every last week of the year" $ forAllValid $ \tod ->
        yearlyDateTimeNextRecurrence (l (d 2019 12 31) tod) limit (Interval 1) [] Monday [WeekNo (-1)] [] [] [] [] [] [] []
          `shouldBe` Just (l (d 2020 12 28) tod)
      specify "Every sixth week in february" $ forAllValid $ \tod ->
        yearlyDateTimeNextRecurrence (l (d 2025 02 09) tod) limit (Interval 1) [February] Monday [WeekNo 6] [] [] [] [] [] [] []
          `shouldBe` Just (l (d 2026 02 02) tod)
  --   -- No 'ByWeekNo' because it's excluded by the table
  --   -- No 'ByYearDay' because it's excluded by the table
  --   describe "ByDay" $ do
  --     specify "Every wednesday and thursday" $ forAllValid $ \tod ->
  --       yearlyDateTimeNextRecurrence (l (d 2020 08 12) tod) limit (Interval 1) [] [] [Every Wednesday, Every Thursday] [] [] [] [] [] [] []
  --         `shouldBe` Just (l (d 2020 08 13) tod)
  --     specify "Every other thursday and friday" $ forAllValid $ \tod ->
  --       yearlyDateTimeNextRecurrence (l (d 2020 08 28) tod) limit (Interval 2) [] [] [Every Thursday, Every Friday] [] [] [] [] [] [] []
  --         `shouldBe` Just (l (d 2020 10 01) tod)
  --     specify "Every sunday, at the end of the year" $ forAllValid $ \tod ->
  --       yearlyDateTimeNextRecurrence (l (d 2019 12 29) tod) limit (Interval 2) [] [] [Every Sunday] [] [] [] [] [] [] []
  --         `shouldBe` Just (l (d 2020 02 02) tod)
  --     specify "Every other sunday, at the end of the year" $ forAllValid $ \tod ->
  --       yearlyDateTimeNextRecurrence (l (d 2019 12 29) tod) limit (Interval 2) [] [] [Every Sunday] [] [] [] [] [] [] []
  --         `shouldBe` Just (l (d 2020 02 02) tod)
  --     specify "Every first wednesday of the year" $ forAllValid $ \tod ->
  --       yearlyDateTimeNextRecurrence (l (d 2020 08 05) tod) limit (Interval 1) [] [] [Specific 1 Wednesday] [] [] [] [] [] [] []
  --         `shouldBe` Just (l (d 2020 09 02) tod)
  --     specify "Every other first thursday" $ forAllValid $ \tod ->
  --       yearlyDateTimeNextRecurrence (l (d 2020 08 06) tod) limit (Interval 2) [] [] [Specific 1 Thursday] [] [] [] [] [] [] []
  --         `shouldBe` Just (l (d 2020 10 01) tod)
  --     specify "Every last wednesday of the year" $ forAllValid $ \tod ->
  --       yearlyDateTimeNextRecurrence (l (d 2020 08 26) tod) limit (Interval 1) [] [] [Specific (-1) Wednesday] [] [] [] [] [] [] []
  --         `shouldBe` Just (l (d 2020 09 30) tod)
  --     specify "Every wednesday that falls on the 10th of the year" $ forAllValid $ \tod ->
  --       yearlyDateTimeNextRecurrence (l (d 2019 07 10) tod) limit (Interval 1) [] [YearDay 10] [Every Wednesday] [] [] [] [] [] [] []
  --         `shouldBe` Just (l (d 2020 06 10) tod)
  --     specify "Every second wednesday that falls on the 10th of the year" $ forAllValid $ \tod ->
  --       yearlyDateTimeNextRecurrence (l (d 2019 07 10) tod) limit (Interval 1) [] [YearDay 10] [Specific 2 Wednesday] [] [] [] [] [] [] []
  --         `shouldBe` Just (l (d 2020 06 10) tod)
  --     specify "Every last wednesday that falls on the 27th of the year" $ forAllValid $ \tod ->
  --       yearlyDateTimeNextRecurrence (l (d 2019 03 27) tod) limit (Interval 1) [] [YearDay 27] [Specific (-1) Wednesday] [] [] [] [] [] [] []
  --         `shouldBe` Just (l (d 2019 11 27) tod)
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
    let limit = d 2028 01 01
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
--   -- No 'ByWeekNo' because it's excluded by the table
--   -- No 'ByYearDay' because it's excluded by the table
--   describe "ByDay" $ do
--     specify "Every wednesday and thursday" $
--       yearlyDateNextRecurrence (d 2020 08 12) limit (Interval 1) [] [] [Every Wednesday, Every Thursday] [] [] [] []
--         `shouldBe` Just (d 2020 08 13)
--     specify "Every other thursday and friday" $
--       yearlyDateNextRecurrence (d 2020 08 28) limit (Interval 2) [] [] [Every Thursday, Every Friday] [] [] [] []
--         `shouldBe` Just (d 2020 10 01)
--     specify "Every sunday, at the end of the year" $
--       yearlyDateNextRecurrence (d 2019 12 29) limit (Interval 2) [] [] [Every Sunday] [] [] [] []
--         `shouldBe` Just (d 2020 02 02)
--     specify "Every other year, ever sunday, at the end of the year" $
--       yearlyDateNextRecurrence (d 2019 12 29) limit (Interval 2) [] [] [Every Sunday] [] [] [] []
--         `shouldBe` Just (d 2020 02 02)
--     specify "Every first wednesday of the year" $
--       yearlyDateNextRecurrence (d 2020 08 05) limit (Interval 1) [] [] [Specific 1 Wednesday] [] [] [] []
--         `shouldBe` Just (d 2020 09 02)
--     specify "Every other first thursday" $
--       yearlyDateNextRecurrence (d 2020 08 06) limit (Interval 2) [] [] [Specific 1 Thursday] [] [] [] []
--         `shouldBe` Just (d 2020 10 01)
--     specify "Every last wednesday of the year" $
--       yearlyDateNextRecurrence (d 2020 08 26) limit (Interval 1) [] [] [Specific (-1) Wednesday] [] [] [] []
--         `shouldBe` Just (d 2020 09 30)
--     specify "Every wednesday that falls on the 10th of the year" $
--       yearlyDateNextRecurrence (d 2019 07 10) limit (Interval 1) [] [YearDay 10] [Every Wednesday] [] [] [] []
--         `shouldBe` Just (d 2020 06 10)
--     specify "Every second wednesday that falls on the 10th of the year" $
--       yearlyDateNextRecurrence (d 2019 07 10) limit (Interval 1) [] [YearDay 10] [Specific 2 Wednesday] [] [] [] []
--         `shouldBe` Just (d 2020 06 10)
--     specify "Every last wednesday that falls on the 27th of the year" $
--       yearlyDateNextRecurrence (d 2019 03 27) limit (Interval 1) [] [YearDay 27] [Specific (-1) Wednesday] [] [] [] []
--         `shouldBe` Just (d 2019 11 27)
--   describe "BySetPos" $ do
--     specify "This special case" $
--       yearlyDateNextRecurrence (d 2021 08 31) limit (Interval 2) [March, April, August] [] [Every Tuesday, Every Sunday] [SetPos (-1)] [] [] []
--         `shouldBe` Just (d 2022 04 26)
