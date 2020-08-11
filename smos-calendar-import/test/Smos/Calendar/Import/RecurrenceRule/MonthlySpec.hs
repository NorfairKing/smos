{-# LANGUAGE OverloadedLists #-}

module Smos.Calendar.Import.RecurrenceRule.MonthlySpec
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
  let l = LocalTime
  let t = TimeOfDay
  describe "monthlyyDateTimeNextRecurrence" $ do
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
    -- No 'ByMonthDay' because it's excluded by the table
    describe "ByDay" $ do
      specify "Every wednesday and thursday" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2020 08 12) tod) limit (Interval 1) [] [] [Every Wednesday, Every Thursday] [] [] [] []
          `shouldBe` Just (l (d 2020 08 13) tod)
      specify "Every other thursday and friday" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2020 08 28) tod) limit (Interval 2) [] [] [Every Thursday, Every Friday] [] [] [] []
          `shouldBe` Just (l (d 2020 10 01) tod)
      specify "Every sunday, at the end of the year" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2019 12 29) tod) limit (Interval 2) [] [] [Every Sunday] [] [] [] []
          `shouldBe` Just (l (d 2020 01 05) tod)
      specify "Every other sunday, at the end of the year" $ forAllValid $ \tod ->
        monthlyDateTimeNextRecurrence (l (d 2019 12 29) tod) limit (Interval 2) [] [] [Every Sunday] [] [] [] []
          `shouldBe` Just (l (d 2020 02 02) tod)
