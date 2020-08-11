{-# LANGUAGE OverloadedLists #-}

module Smos.Calendar.Import.RecurrenceRule.MonthlySpec
  ( spec,
  )
where

import Data.Time
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurrenceRule.Gen ()
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Monthly
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  let d = fromGregorian
  let l = LocalTime
  describe "monthlyDateTimeNextRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = l (d 2021 01 01) midnight
    describe "No ByX's" $ do
      specify "Every month" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2020 08 08) tod) limit (Interval 1) [] [] [] [] [] [] []
          `shouldBe` Just (l (d 2020 09 08) tod)
      specify "Every other month" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2020 08 08) tod) limit (Interval 2) [] [] [] [] [] [] []
          `shouldBe` Just (l (d 2020 10 08) tod)
    describe "ByMonth" $ do
      specify "Every month in Sept" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2019 09 30) tod) limit (Interval 1) [September] [] [] [] [] [] []
          `shouldBe` Just (l (d 2020 09 30) tod)
      specify "Every other month in Sept" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2019 09 30) tod) limit (Interval 2) [September] [] [] [] [] [] []
          `shouldBe` Just (l (d 2020 09 30) tod)
      specify "Every five months in Sept" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2015 09 30) tod) limit (Interval 5) [September] [] [] [] [] [] []
          `shouldBe` Just (l (d 2020 09 30) tod)
    -- No 'ByWeekNo' because it's excluded by the table
    -- No 'ByYearDay' because it's excluded by the table
    describe "ByDay" $ do
      specify "Every wednesday and thursday" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2020 08 12) tod) limit (Interval 1) [] [] [Every Wednesday, Every Thursday] [] [] [] []
          `shouldBe` Just (l (d 2020 08 13) tod)
      specify "Every other thursday and friday" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2020 08 28) tod) limit (Interval 2) [] [] [Every Thursday, Every Friday] [] [] [] []
          `shouldBe` Just (l (d 2020 10 01) tod)
      specify "Every sunday, at the end of the year" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2019 12 29) tod) limit (Interval 2) [] [] [Every Sunday] [] [] [] []
          `shouldBe` Just (l (d 2020 02 02) tod)
      specify "Every other sunday, at the end of the year" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2019 12 29) tod) limit (Interval 2) [] [] [Every Sunday] [] [] [] []
          `shouldBe` Just (l (d 2020 02 02) tod)
      specify "Every first wednesday of the month" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2020 08 05) tod) limit (Interval 1) [] [] [Specific 1 Wednesday] [] [] [] []
          `shouldBe` Just (l (d 2020 09 02) tod)
      specify "Every other first thursday" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2020 08 06) tod) limit (Interval 2) [] [] [Specific 1 Thursday] [] [] [] []
          `shouldBe` Just (l (d 2020 10 01) tod)
      specify "Every last wednesday of the month" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2020 08 26) tod) limit (Interval 1) [] [] [Specific (-1) Wednesday] [] [] [] []
          `shouldBe` Just (l (d 2020 09 30) tod)
  describe "monthlyDateNextRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = d 2021 01 01
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
