{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.RecurrenceRuleSpec
  ( spec,
  )
where

import qualified Data.Set as S
import Data.Time
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurrenceRule.Gen ()
import Smos.Calendar.Import.UnresolvedTimestamp
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @RRule
  describe "Examples from the spec in section 3.8.5.3" $ do
    it "Daily for 10 occurrences" $ do
      -- DTSTART;TZID=America/New_York:19970902T090000
      -- RRULE:FREQ=DAILY;COUNT=10

      -- ==> (1997 9:00 AM EDT) September 2-11
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule = (rRule Daily) {rRuleUntilCount = Count 10}
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
      rruleOccurrencesUntil dtstart rule limit
        `shouldBe` S.fromList (map (\d -> LocalTime (fromGregorian 1997 09 d) (TimeOfDay 09 00 00)) [2 .. 11])
