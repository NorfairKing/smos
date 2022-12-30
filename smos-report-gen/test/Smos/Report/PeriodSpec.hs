{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Report.PeriodSpec (spec) where

import Data.Time
import Smos.Report.Period
import Smos.Report.Period.Gen ()
import Smos.Report.TimeBlock
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @Period
  let d = fromGregorian
  describe "periodInterval" $ do
    it "produces valid intervals" $
      producesValid2 periodInterval

    it "works for LastMonth at the beginning of the year" $
      periodInterval (d 2023 01 05) LastMonth `shouldBe` Interval (d 2022 12 01) (d 2023 01 01)
    it "works for NextMonth at the end of the year" $
      periodInterval (d 2022 12 05) NextMonth `shouldBe` Interval (d 2023 01 01) (d 2023 02 01)

    it "works for LastWeek at the beginning of the year" $
      periodInterval (d 2023 01 02) LastWeek `shouldBe` Interval (d 2022 12 26) (d 2023 01 02)
    it "works for ThisWeek at the beginning of the year" $
      periodInterval (d 2023 01 01) ThisWeek `shouldBe` Interval (d 2022 12 26) (d 2023 01 02)
    it "works for NextWeek at the end of the year" $
      periodInterval (d 2022 12 30) NextWeek `shouldBe` Interval (d 2023 01 02) (d 2023 01 09)

  describe "yearInterval" $ do
    it "works for 2022" $
      yearInterval 2022 `shouldBe` Interval (d 2022 01 01) (d 2023 01 01)

  describe "monthInterval" $ do
    it "works for Jan 2022" $
      monthInterval (MonthNumber 2022 01) `shouldBe` Interval (d 2022 01 01) (d 2022 02 01)
    it "works for Dec 2022" $
      monthInterval (MonthNumber 2022 12) `shouldBe` Interval (d 2022 12 01) (d 2023 01 01)

  describe "weekInterval" $ do
    it "works for the first week of 2023" $
      weekInterval (WeekNumber 2023 01) `shouldBe` Interval (d 2023 01 02) (d 2023 01 09)
    it "works for the 52th week of 2022" $
      weekInterval (WeekNumber 2022 52) `shouldBe` Interval (d 2022 12 26) (d 2023 01 02)

  describe "dayInterval" $ do
    it "works for 31 dec 2022" $
      dayInterval (d 2022 12 31) `shouldBe` Interval (d 2022 12 31) (d 2023 01 01)
    it "works for 1 jan 2022" $
      dayInterval (d 2022 01 01) `shouldBe` Interval (d 2022 01 01) (d 2022 01 02)
