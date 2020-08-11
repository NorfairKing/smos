{-# LANGUAGE OverloadedLists #-}

module Smos.Calendar.Import.RecurrenceRule.WeeklySpec
  ( spec,
  )
where

import Data.Time
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurrenceRule.Gen ()
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Weekly
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  let d = fromGregorian
  let l = LocalTime
  let t = TimeOfDay
  describe "weeklyyDateTimeNextRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = l (d 2021 01 01) midnight
    describe "No ByX's" $ do
      specify "Every week" $ forAllValid $ \tod ->
        weeklyDateTimeNextRecurrence (l (d 2020 08 08) tod) limit (Interval 1) [] Monday [] [] [] [] []
          `shouldBe` Just (l (d 2020 08 15) tod)
      specify "Every other week" $ forAllValid $ \tod ->
        weeklyDateTimeNextRecurrence (l (d 2020 08 08) tod) limit (Interval 2) [] Monday [] [] [] [] []
          `shouldBe` Just (l (d 2020 08 22) tod)
    describe "ByMonth" $ do
      specify "Every week in Sept" $ forAllValid $ \tod ->
        weeklyDateTimeNextRecurrence (l (d 2019 09 30) tod) limit (Interval 1) [September] Monday [] [] [] [] []
          `shouldBe` Just (l (d 2020 09 07) tod)
      specify "Every other week in Sept" $ forAllValid $ \tod ->
        weeklyDateTimeNextRecurrence (l (d 2019 09 30) tod) limit (Interval 2) [September] Monday [] [] [] [] []
          `shouldBe` Just (l (d 2020 09 14) tod)
    -- No 'ByWeekNo' because it's excluded by the table
    -- No 'ByYearDay' because it's excluded by the table
    -- No 'ByMonthDay' because it's excluded by the table
    describe "ByDay" $ do
      specify "Every wednesday and thursday" $ forAllValid $ \tod ->
        weeklyDateTimeNextRecurrence (l (d 2020 08 05) tod) limit (Interval 1) [] Monday [Wednesday, Thursday] [] [] [] []
          `shouldBe` Just (l (d 2020 08 06) tod)
      specify "Every other thursday and friday" $ forAllValid $ \tod ->
        weeklyDateTimeNextRecurrence (l (d 2020 08 07) tod) limit (Interval 2) [] Monday [Thursday, Friday] [] [] [] []
          `shouldBe` Just (l (d 2020 08 20) tod)
      specify "Every sunday, at the end of the year" $ forAllValid $ \tod ->
        weeklyDateTimeNextRecurrence (l (d 2019 12 29) tod) limit (Interval 1) [] Monday [Sunday] [] [] [] []
          `shouldBe` Just (l (d 2020 01 05) tod)
    describe "BySetPos" $ do
      specify "The first day of every week" $ forAllValid $ \tod ->
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
  describe "weeklyyDateNextRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = d 2021 01 01
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
