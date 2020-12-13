{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.RecurrenceRuleSpec
  ( spec,
  )
where

import qualified Data.Set as S
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurrenceRule.Gen ()
import Test.Syd
import Test.QuickCheck
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Frequency
  jsonSpecOnValid @Frequency
  genValidSpec @UntilCount
  jsonSpecOnValid @UntilCount
  genValidSpec @Interval
  jsonSpecOnValid @Interval
  genValidSpec @BySecond
  jsonSpecOnValid @BySecond
  genValidSpec @ByMinute
  jsonSpecOnValid @ByMinute
  genValidSpec @ByHour
  jsonSpecOnValid @ByHour
  genValidSpec @ByDay
  jsonSpecOnValid @ByDay
  genValidSpec @ByMonthDay
  jsonSpecOnValid @ByMonthDay
  genValidSpec @ByYearDay
  jsonSpecOnValid @ByYearDay
  genValidSpec @ByWeekNo
  jsonSpecOnValid @ByWeekNo
  genValidSpec @Month
  jsonSpecOnValid @Month
  genValidSpec @BySetPos
  jsonSpecOnValid @BySetPos
  genValidSpec @RRule
  jsonSpecOnValid @RRule
  describe "rruleDateTimeOccurrencesUntil" $ do
    xit "produces valid results" $ producesValidsOnValids3 rruleDateTimeOccurrencesUntil
    xit "produces results within the 'Count' range for 'Count' rules" $
      forAllValid $ \start ->
        forAllValid $ \limit ->
          forAllValid $ \c ->
            forAll (((\r -> r {rRuleUntilCount = Count c}) <$> genValid) `suchThat` isValid) $ \rule ->
              let s = rruleDateTimeOccurrencesUntil start rule limit
               in fromIntegral (S.size s) `shouldSatisfy` (<= c)
