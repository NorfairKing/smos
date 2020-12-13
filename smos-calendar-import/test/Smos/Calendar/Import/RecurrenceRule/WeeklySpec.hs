{-# LANGUAGE OverloadedLists #-}

module Smos.Calendar.Import.RecurrenceRule.WeeklySpec
  ( spec,
  )
where

import Data.Time
import Safe
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurrenceRule.Gen ()
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Weekly
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
              (rRule Weekly)
                { rRuleInterval = Interval 2,
                  rRuleUntilCount = Count 11,
                  rRuleByDay = [Every Wednesday, Every Thursday],
                  rRuleByMonth = [September, November],
                  rRuleWeekStart = Wednesday
                }
            start = LocalTime (d 2020 08 20) tod
         in --  This limit will be reached and cut of 2 recurrences
            rruleDateTimeOccurrencesUntil start rule limit
              `shouldBe` [ LocalTime (d 2020 08 20) tod,
                           LocalTime (d 2020 09 02) tod,
                           LocalTime (d 2020 09 03) tod,
                           LocalTime (d 2020 09 16) tod,
                           LocalTime (d 2020 09 17) tod,
                           LocalTime (d 2020 09 30) tod,
                           LocalTime (d 2020 11 11) tod,
                           LocalTime (d 2020 11 12) tod,
                           LocalTime (d 2020 11 25) tod,
                           LocalTime (d 2020 11 26) tod,
                           LocalTime (d 2021 09 01) tod
                         ]
    specify "It works for this BYSETPOS example: The last hour of every week" $
      --  An limit in the future because it won't be reached anyway
      let limit = LocalTime (d 2024 01 01) midnight
          rule =
            (rRule Weekly)
              { rRuleInterval = Interval 1,
                rRuleUntilCount = Count 3,
                rRuleBySetPos = [SetPos (-1)],
                rRuleByHour =
                  [ Hour 22,
                    Hour 23
                  ],
                rRuleByMinute = [Minute 0],
                rRuleBySecond = [Second 0],
                rRuleWeekStart = Monday
              }
          start = LocalTime (d 2020 08 09) (t 23 00 00)
       in rruleDateTimeOccurrencesUntil start rule limit
            `shouldBe` [ LocalTime (d 2020 08 09) (t 23 00 00),
                         LocalTime (d 2020 08 16) (t 23 00 00),
                         LocalTime (d 2020 08 23) (t 23 00 00)
                       ]
    specify "It works for this BYSETPOS example: The last two hours of every week" $
      --  An limit in the future because it won't be reached anyway
      let limit = LocalTime (d 2024 01 01) midnight
          rule =
            (rRule Weekly)
              { rRuleInterval = Interval 1,
                rRuleUntilCount = Count 4,
                rRuleBySetPos = [SetPos (-1), SetPos (-2)],
                rRuleByHour =
                  [ Hour 22,
                    Hour 23
                  ],
                rRuleByMinute = [Minute 0],
                rRuleBySecond = [Second 0],
                rRuleWeekStart = Monday
              }
          start = LocalTime (d 2020 08 09) (t 22 00 00)
       in rruleDateTimeOccurrencesUntil start rule limit
            `shouldBe` [ LocalTime (d 2020 08 09) (t 22 00 00),
                         LocalTime (d 2020 08 09) (t 23 00 00),
                         LocalTime (d 2020 08 16) (t 22 00 00),
                         LocalTime (d 2020 08 16) (t 23 00 00)
                       ]
  describe "weeklyDateTimeRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = l (d 2021 01 01) midnight
    let weeklyDateTimeNextRecurrence start lim i ba bb bc bd be bf bg =
          headMay $ weeklyDateTimeRecurrence start lim i ba bb bc bd be bf bg
    describe "No ByX's" $ do
      specify "Every week" $
        forAllValid $ \tod ->
          weeklyDateTimeNextRecurrence (l (d 2020 08 08) tod) limit (Interval 1) [] Monday [] [] [] [] []
            `shouldBe` Just (l (d 2020 08 15) tod)
      specify "Every other week" $
        forAllValid $ \tod ->
          weeklyDateTimeNextRecurrence (l (d 2020 08 08) tod) limit (Interval 2) [] Monday [] [] [] [] []
            `shouldBe` Just (l (d 2020 08 22) tod)
    describe "ByMonth" $ do
      specify "Every week in Sept" $
        forAllValid $ \tod ->
          weeklyDateTimeNextRecurrence (l (d 2019 09 30) tod) limit (Interval 1) [September] Monday [] [] [] [] []
            `shouldBe` Just (l (d 2020 09 07) tod)
      specify "Every other week in Sept" $
        forAllValid $ \tod ->
          weeklyDateTimeNextRecurrence (l (d 2019 09 30) tod) limit (Interval 2) [September] Monday [] [] [] [] []
            `shouldBe` Just (l (d 2020 09 14) tod)
    -- No 'ByWeekNo' because it's excluded by the table
    -- No 'ByYearDay' because it's excluded by the table
    -- No 'ByMonthDay' because it's excluded by the table
    describe "ByDay" $ do
      specify "Every wednesday and thursday" $
        forAllValid $ \tod ->
          weeklyDateTimeNextRecurrence (l (d 2020 08 05) tod) limit (Interval 1) [] Monday [Wednesday, Thursday] [] [] [] []
            `shouldBe` Just (l (d 2020 08 06) tod)
      specify "Every other thursday and friday" $
        forAllValid $ \tod ->
          weeklyDateTimeNextRecurrence (l (d 2020 08 07) tod) limit (Interval 2) [] Monday [Thursday, Friday] [] [] [] []
            `shouldBe` Just (l (d 2020 08 20) tod)
      specify "Every sunday, at the end of the year" $
        forAllValid $ \tod ->
          weeklyDateTimeNextRecurrence (l (d 2019 12 29) tod) limit (Interval 1) [] Monday [Sunday] [] [] [] []
            `shouldBe` Just (l (d 2020 01 05) tod)
    describe "BySetPos" $ do
      specify "The first day of every week" $
        forAllValid $ \tod ->
          weeklyDateTimeNextRecurrence (l (d 2020 08 07) tod) limit (Interval 1) [] Friday [Friday, Saturday] [] [] [] [SetPos 1]
            `shouldBe` Just (l (d 2020 08 14) tod)
    describe "ByHour" $ do
      specify "16h every other week" $
        weeklyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 16 00 00)) limit (Interval 2) [] Monday [] [Hour 16] [] [] []
          `shouldBe` Just (LocalTime (d 2020 08 20) (t 16 00 00))
    describe "ByMinute" $ do
      specify "16h20 every third week" $
        weeklyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 16 20 00)) limit (Interval 3) [] Monday [] [Hour 16] [Minute 20] [] []
          `shouldBe` Just (LocalTime (d 2020 08 27) (t 16 20 00))
    describe "BySecond" $ do
      specify "16h20m30s every fourth week" $
        weeklyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 15 00 00)) limit (Interval 4) [] Monday [] [Hour 16] [Minute 20] [Second 30] []
          `shouldBe` Just (LocalTime (d 2020 08 06) (t 16 20 30))
      specify "every 15th and 20th second" $
        weeklyDateTimeNextRecurrence (LocalTime (d 2020 08 06) (t 15 00 15)) limit (Interval 1) [] Monday [] [] [] [Second 15, Second 20] []
          `shouldBe` Just (LocalTime (d 2020 08 06) (t 15 00 20))
  describe "weeklyDateRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = d 2021 01 01
    let weeklyDateNextRecurrence start lim i ba bb bc bd =
          headMay $ weeklyDateRecurrence start lim i ba bb bc bd
    describe "No ByX's" $ do
      specify "Every week" $
        weeklyDateNextRecurrence (d 2020 08 08) limit (Interval 1) [] Monday [] []
          `shouldBe` Just (d 2020 08 15)
      specify "Every other week" $
        weeklyDateNextRecurrence (d 2020 08 08) limit (Interval 2) [] Monday [] []
          `shouldBe` Just (d 2020 08 22)
    describe "ByMonth" $ do
      specify "Every week in Sept" $
        weeklyDateNextRecurrence (d 2019 09 30) limit (Interval 1) [September] Monday [] []
          `shouldBe` Just (d 2020 09 07)
      specify "Every other week in Sept" $
        weeklyDateNextRecurrence (d 2019 09 30) limit (Interval 2) [September] Monday [] []
          `shouldBe` Just (d 2020 09 14)
    -- No 'ByWeekNo' because it's excluded by the table
    -- No 'ByYearDay' because it's excluded by the table
    -- No 'ByMonthDay' because it's excluded by the table
    describe "ByDay" $ do
      specify "Every wednesday and thursday" $
        weeklyDateNextRecurrence (d 2020 08 05) limit (Interval 1) [] Monday [Wednesday, Thursday] []
          `shouldBe` Just (d 2020 08 06)
      specify "Every other thursday and friday" $
        weeklyDateNextRecurrence (d 2020 08 07) limit (Interval 2) [] Monday [Thursday, Friday] []
          `shouldBe` Just (d 2020 08 20)
      specify "Every sunday, at the end of the year" $
        weeklyDateNextRecurrence (d 2019 12 29) limit (Interval 1) [] Monday [Sunday] []
          `shouldBe` Just (d 2020 01 05)
      specify "Every other tuesday and sunday with the week starting on monday" $ do
        weeklyDateNextRecurrence (d 1997 08 05) limit (Interval 2) [] Monday [Tuesday, Sunday] []
          `shouldBe` Just (d 1997 08 10)
        weeklyDateNextRecurrence (d 1997 08 10) limit (Interval 2) [] Monday [Tuesday, Sunday] []
          `shouldBe` Just (d 1997 08 19)
        weeklyDateNextRecurrence (d 1997 08 19) limit (Interval 2) [] Monday [Tuesday, Sunday] []
          `shouldBe` Just (d 1997 08 24)
      specify "Every other tuesday and sunday with the week starting on sunday" $ do
        weeklyDateNextRecurrence (d 1997 08 05) limit (Interval 2) [] Sunday [Tuesday, Sunday] []
          `shouldBe` Just (d 1997 08 17)
        weeklyDateNextRecurrence (d 1997 08 17) limit (Interval 2) [] Sunday [Tuesday, Sunday] []
          `shouldBe` Just (d 1997 08 19)
        weeklyDateNextRecurrence (d 1997 08 19) limit (Interval 2) [] Sunday [Tuesday, Sunday] []
          `shouldBe` Just (d 1997 08 31)
    describe "BySetPos" $ do
      specify "The last day of every week" $
        weeklyDateNextRecurrence (d 2020 08 07) limit (Interval 1) [] Friday [Friday, Saturday] [SetPos 1]
          `shouldBe` Just (d 2020 08 14)
      specify "The last day of every week in september" $
        weeklyDateNextRecurrence (d 2020 09 05) limit (Interval 1) [] Sunday [Friday, Saturday] [SetPos (- 1)]
          `shouldBe` Just (d 2020 09 12)
