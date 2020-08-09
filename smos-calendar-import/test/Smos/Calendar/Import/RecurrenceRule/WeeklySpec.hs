{-# LANGUAGE OverloadedLists #-}

module Smos.Calendar.Import.RecurrenceRule.WeeklySpec
  ( spec,
  )
where

import Data.Time
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurrenceRule.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  let d = fromGregorian
  describe "firstDayOfTheWSWeekThatContainsJan1st" $ do
    it "works for a day in 2014 with a Monday week start" $
      firstDayOfTheWSWeekThatContainsJan1st Monday 2014 `shouldBe` d 2013 12 30
    it "works for a day in 2015 with a Monday week start" $
      firstDayOfTheWSWeekThatContainsJan1st Monday 2015 `shouldBe` d 2014 12 29
    it "works for a day in 2014 with a Sunday week start" $
      firstDayOfTheWSWeekThatContainsJan1st Sunday 2014 `shouldBe` d 2013 12 29
    it "works for a day in 2015 with a Sunday week start" $
      firstDayOfTheWSWeekThatContainsJan1st Sunday 2015 `shouldBe` d 2014 12 28
    it "works for a day in 2014 with a Thursday week start" $
      firstDayOfTheWSWeekThatContainsJan1st Thursday 2014 `shouldBe` d 2013 12 26
    it "works for a day in 2015 with a Thursday week start" $
      firstDayOfTheWSWeekThatContainsJan1st Thursday 2015 `shouldBe` d 2015 01 01
  describe "toWeekDateWithStart" $ do
    describe "examples" $ do
      it "works for 2013-12-29 in 2013 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 2013 12 29) `shouldBe` (2013, WeekNo 52, Sunday)
      it "works for 2014-01-01 in 2014 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 2014 01 01) `shouldBe` (2014, WeekNo 01, Wednesday)
      it "works for 2014-01-05 in 2014 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 2014 01 05) `shouldBe` (2014, WeekNo 01, Sunday)
      it "works for 2013-12-29 in 2013 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 2013 12 29) `shouldBe` (2014, WeekNo 01, Sunday)
      it "works for 2014-01-01 in 2014 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 2014 01 01) `shouldBe` (2014, WeekNo 01, Wednesday)
      it "works for 2014-01-05 in 2014 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 2014 01 05) `shouldBe` (2014, WeekNo 02, Sunday)
      it "works for 2014-12-29 in 2015 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 2014 12 29) `shouldBe` (2015, WeekNo 01, Monday)
      it "works for 2015-01-01 in 2015 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 2015 01 01) `shouldBe` (2015, WeekNo 01, Thursday)
      it "works for 2015-01-05 in 2015 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 2015 01 05) `shouldBe` (2015, WeekNo 02, Monday)
      it "works for 2014-12-29 in 2014 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 2014 12 29) `shouldBe` (2014, WeekNo 53, Monday)
      it "works for 2015-01-01 in 2014 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 2015 01 01) `shouldBe` (2014, WeekNo 53, Thursday)
      it "works for 2015-01-05 in 2015 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 2015 01 05) `shouldBe` (2015, WeekNo 01, Monday)
      it "works for 1997-01-01 in 1997 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 1997 01 01) `shouldBe` (1997, WeekNo 01, Wednesday)
      it "works for 1997-01-07 in 1997 with a Monday weekstart" $
        toWeekDateWithStart Monday (d 1997 01 07) `shouldBe` (1997, WeekNo 02, Tuesday)
      it "works for 1997-01-01 in 1997 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 1997 01 01) `shouldBe` (1997, WeekNo 01, Wednesday)
      it "works for 1997-01-07 in 1997 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 1997 01 07) `shouldBe` (1997, WeekNo 02, Tuesday)
      it "works for 1997-08-17 in 1997 with a Sunday weekstart" $
        toWeekDateWithStart Sunday (d 1997 08 17) `shouldBe` (1997, WeekNo 34, Sunday)
      it "works for 1997-08-17 in 1997 with a Sunday weekstart" $
        toWeekDateWithStart Monday (d 1997 08 17) `shouldBe` (1997, WeekNo 33, Sunday)
    it "produces tuples of which the last value is independent of the week start" $ forAllValid $ \ws1 ->
      forAllValid $ \ws2 ->
        forAllValid $ \day ->
          let (_, _, dow1) = toWeekDateWithStart ws1 day
              (_, _, dow2) = toWeekDateWithStart ws2 day
           in dow1 `shouldBe` dow2
    it "produces valid results" $ producesValidsOnValids2 toWeekDateWithStart
  describe "fromWeekDateWithStart" $ do
    describe "examples" $ do
      it "works for 2013-12-29 in 2013 with a Monday weekstart" $
        fromWeekDateWithStart Monday 2013 (WeekNo 52) Sunday `shouldBe` Just (d 2013 12 29)
      it "works for 2014-01-01 in 2014 with a Monday weekstart" $
        fromWeekDateWithStart Monday 2014 (WeekNo 01) Wednesday `shouldBe` Just (d 2014 01 01)
      it "works for 2014-01-05 in 2014 with a Monday weekstart" $
        fromWeekDateWithStart Monday 2014 (WeekNo 01) Sunday `shouldBe` Just (d 2014 01 05)
      it "works for 2013-12-29 in 2013 with a Sunday weekstart" $
        fromWeekDateWithStart Sunday 2014 (WeekNo 01) Sunday `shouldBe` Just (d 2013 12 29)
      it "works for 2014-01-01 in 2014 with a Sunday weekstart" $
        fromWeekDateWithStart Sunday 2014 (WeekNo 01) Wednesday `shouldBe` Just (d 2014 01 01)
      it "works for 2014-01-05 in 2014 with a Sunday weekstart" $
        fromWeekDateWithStart Sunday 2014 (WeekNo 02) Sunday `shouldBe` Just (d 2014 01 05)
      it "works for 2014-12-29 in 2015 with a Monday weekstart" $
        fromWeekDateWithStart Monday 2015 (WeekNo 01) Monday `shouldBe` Just (d 2014 12 29)
      it "works for 2015-01-01 in 2015 with a Monday weekstart" $
        fromWeekDateWithStart Monday 2015 (WeekNo 01) Thursday `shouldBe` Just (d 2015 01 01)
      it "works for 2015-01-05 in 2015 with a Monday weekstart" $
        fromWeekDateWithStart Monday 2015 (WeekNo 02) Monday `shouldBe` Just (d 2015 01 05)
      it "works for 2014-12-29 in 2014 with a Sunday weekstart" $
        fromWeekDateWithStart Sunday 2014 (WeekNo 53) Monday `shouldBe` Just (d 2014 12 29)
      it "works for 2015-01-01 in 2014 with a Sunday weekstart" $
        fromWeekDateWithStart Sunday 2014 (WeekNo 53) Thursday `shouldBe` Just (d 2015 01 01)
      it "works for 2015-01-05 in 2015 with a Sunday weekstart" $
        fromWeekDateWithStart Sunday 2015 (WeekNo 01) Monday `shouldBe` Just (d 2015 01 05)
    it "produces valid results" $ forAllValid $ producesValidsOnValids3 . fromWeekDateWithStart
    xdescribe "This fails for very negative years for unknown reasons"
      $ it "roundtrips toWeekDateWithStart for a given week start"
      $ forAllValid
      $ \ws ->
        forAllValid $ \day ->
          let (y, wn, dow) = toWeekDateWithStart ws day
           in fromWeekDateWithStart ws y wn dow `shouldBe` Just day
  let l = LocalTime
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
