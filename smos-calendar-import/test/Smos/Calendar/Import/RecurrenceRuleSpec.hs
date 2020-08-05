{-# LANGUAGE LambdaCase #-}
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
    specify "Daily for 10 occurrences" $ do
      -- DTSTART;TZID=America/New_York:19970902T090000
      -- RRULE:FREQ=DAILY;COUNT=10
      --
      -- ==> (1997 9:00 AM EDT) September 2-11
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule = (rRule Daily) {rRuleUntilCount = Count 10}
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
      rruleOccurrencesUntil dtstart rule limit
        `shouldBe` S.fromList (map (\d -> LocalTime (fromGregorian 1997 09 d) (TimeOfDay 09 00 00)) [2 .. 11])
    specify "Daily until December 24, 1997" $ do
      --  DTSTART;TZID=America/New_York:19970902T090000
      --  RRULE:FREQ=DAILY;UNTIL=19971224T000000Z
      --
      --  ==> (1997 9:00 AM EDT) September 2-30;October 1-25
      --      (1997 9:00 AM EST) October 26-31;November 1-30;December 1-23
      --
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule = (rRule Daily) {rRuleUntilCount = Until (LocalTime (fromGregorian 1997 12 24) (TimeOfDay 00 00 00))}
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
          from = fromGregorian 1997 09 02
          to = fromGregorian 1997 12 23
      rruleOccurrencesUntil dtstart rule limit
        `shouldBe` S.fromList (map (\d -> LocalTime d (TimeOfDay 09 00 00)) [from .. to])
    specify "Every other day - forever" $ do
      -- Every other day - forever:
      --
      --  DTSTART;TZID=America/New_York:19970902T090000
      --  RRULE:FREQ=DAILY;INTERVAL=2
      --
      --  ==> (1997 9:00 AM EDT) September 2,4,6,8...24,26,28,30;
      --                         October 2,4,6...20,22,24
      --      (1997 9:00 AM EST) October 26,28,30;
      --                         November 1,3,5,7...25,27,29;
      --                         December 1,3,...
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule = (rRule Daily) {rRuleInterval = 2}
          limit = LocalTime (fromGregorian 1997 12 03) (TimeOfDay 09 00 00)
          from = fromGregorian 1997 09 02
          to = fromGregorian 1997 12 03
          everySecond = go
            where
              go = \case
                [] -> []
                [x] -> [x]
                (x : y : zs) -> x : go zs
      rruleOccurrencesUntil dtstart rule limit
        `shouldBe` S.fromList (map (\d -> LocalTime d (TimeOfDay 09 00 00)) (everySecond [from .. to]))
