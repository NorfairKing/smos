{-# LANGUAGE OverloadedLists #-}

module Smos.Calendar.Import.RecurrenceRule.WeeklySpec
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
  describe "weeklyyDateNextRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let d = fromGregorian
    let limit = d 2021 01 01
    describe "No ByX's" $ do
      specify "Every week" $
        weeklyDateNextRecurrence (d 2020 08 08) limit (Interval 1) [] [] []
          `shouldBe` Just (d 2020 08 15)
      specify "Every other week" $
        weeklyDateNextRecurrence (d 2020 08 08) limit (Interval 2) [] [] []
          `shouldBe` Just (d 2020 08 22)
    describe "ByMonth" $ do
      specify "Every week in Sept" $
        weeklyDateNextRecurrence (d 2019 09 30) limit (Interval 1) [September] [] []
          `shouldBe` Just (d 2020 09 07)
      specify "Every other week in Sept" $
        weeklyDateNextRecurrence (d 2019 09 30) limit (Interval 2) [September] [] []
          `shouldBe` Just (d 2020 09 14)
    -- No 'ByWeekNo' because it's excluded by the table
    -- No 'ByYearDay' because it's excluded by the table
    -- No 'ByMonthDay' because it's excluded by the table
    describe "ByDay" $ do
      specify "Every wednesday and thursday" $
        weeklyDateNextRecurrence (d 2020 08 05) limit (Interval 1) [] [Wednesday, Thursday] []
          `shouldBe` Just (d 2020 08 06)
      specify "Every other thursday and friday" $
        weeklyDateNextRecurrence (d 2020 08 07) limit (Interval 2) [] [Thursday, Friday] []
          `shouldBe` Just (d 2020 08 20)
