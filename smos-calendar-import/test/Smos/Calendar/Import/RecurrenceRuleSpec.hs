{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.RecurrenceRuleSpec
  ( spec,
  )
where

import qualified Data.Set as S
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurrenceRule.Gen ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Frequency
  jsonSpec @Frequency
  genValidSpec @UntilCount
  jsonSpec @UntilCount
  genValidSpec @Interval
  jsonSpec @Interval
  genValidSpec @BySecond
  jsonSpec @BySecond
  genValidSpec @ByMinute
  jsonSpec @ByMinute
  genValidSpec @ByHour
  jsonSpec @ByHour
  genValidSpec @ByDay
  jsonSpec @ByDay
  genValidSpec @ByMonthDay
  jsonSpec @ByMonthDay
  genValidSpec @ByYearDay
  jsonSpec @ByYearDay
  genValidSpec @ByWeekNo
  jsonSpec @ByWeekNo
  genValidSpec @Month
  jsonSpec @Month
  genValidSpec @BySetPos
  jsonSpec @BySetPos
  genValidSpec @RRule
  jsonSpec @RRule
  describe "rruleDateTimeOccurrencesUntil" $ do
    xit "produces valid results" $ producesValid3 rruleDateTimeOccurrencesUntil
    xit "produces results within the 'Count' range for 'Count' rules" $
      forAllValid $ \start ->
        forAllValid $ \limit ->
          forAllValid $ \c ->
            forAll (((\r -> r {rRuleUntilCount = Count c}) <$> genValid) `suchThat` isValid) $ \rule ->
              let s = rruleDateTimeOccurrencesUntil start rule limit
               in fromIntegral (S.size s) `shouldSatisfy` (<= c)
