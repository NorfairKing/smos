{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.RecurrenceRule.RFC5545Spec
  ( spec,
  )
where

import qualified Data.Set as S
import Data.Time
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurrenceRule.Gen ()
import Test.Syd

spec :: Spec
spec =
  describe "Examples from the spec in section 3.8.5.3" $ do
    specify "Daily for 10 occurrences" $ do
      --
      -- DTSTART;TZID=America/New_York:19970902T090000
      -- RRULE:FREQ=DAILY;COUNT=10
      --
      -- ==> (1997 9:00 AM EDT) September 2-11
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule = (rRule Daily) {rRuleUntilCount = Count 10}
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 1998 01 01) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` S.map (\d -> LocalTime (fromGregorian 1997 09 d) (TimeOfDay 09 00 00)) [2 .. 11]
    specify "Daily until December 24, 1997" $ do
      --
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
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` S.fromList (map (\d -> LocalTime d (TimeOfDay 09 00 00)) [from .. to])
    specify "Every other day - forever" $ do
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
          rule = (rRule Daily) {rRuleInterval = Interval 2}
          limit = LocalTime (fromGregorian 1997 12 03) (TimeOfDay 09 00 00)
          from = fromGregorian 1997 09 02
          to = fromGregorian 1997 12 03
          everySecond = go
            where
              go = \case
                [] -> []
                [x] -> [x]
                (x : _ : zs) -> x : go zs
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` S.fromList (map (\d -> LocalTime d (TimeOfDay 09 00 00)) (everySecond [from .. to]))
    specify "Every 10 days, 5 occurrences" $ do
      --
      --  DTSTART;TZID=America/New_York:19970902T090000
      --  RRULE:FREQ=DAILY;INTERVAL=10;COUNT=5
      --
      --  ==> (1997 9:00 AM EDT) September 2,12,22;
      --                         October 2,12
      --
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule = (rRule Daily) {rRuleInterval = Interval 10, rRuleUntilCount = Count 5}
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 12) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 22) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 12) (TimeOfDay 09 00 00)
                   ]
    specify "Every day in January, for 3 years" $ do
      --
      --  DTSTART;TZID=America/New_York:19980101T090000
      --
      --  RRULE:FREQ=YEARLY;UNTIL=20000131T140000Z;
      --   BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA
      --  or
      --  RRULE:FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1
      --
      --  ==> (1998 9:00 AM EST)January 1-31
      --      (1999 9:00 AM EST)January 1-31
      --      (2000 9:00 AM EST)January 1-31
      --
      let dtstart = LocalTime (fromGregorian 1998 01 01) (TimeOfDay 09 00 00)
          rule =
            (rRule Yearly)
              { rRuleUntilCount = Until (LocalTime (fromGregorian 2000 01 31) (TimeOfDay 14 00 00)),
                rRuleByMonth = [January],
                rRuleByDay =
                  [ Every Sunday,
                    Every Monday,
                    Every Tuesday,
                    Every Wednesday,
                    Every Thursday,
                    Every Friday,
                    Every Saturday
                  ]
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 02 01) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` S.fromList
          ( do
              y <- [1998 .. 2000]
              md <- [1 .. 31]
              pure $ LocalTime (fromGregorian y 01 md) (TimeOfDay 09 00 00)
          )
    specify "Weekly for 10 occurrences" $ do
      --
      --  DTSTART;TZID=America/New_York:19970902T090000
      --  RRULE:FREQ=WEEKLY;COUNT=10
      --
      --  ==> (1997 9:00 AM EDT) September 2,9,16,23,30;October 7,14,21
      --      (1997 9:00 AM EST) October 28;November 4
      --
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule = (rRule Weekly) {rRuleUntilCount = Count 10}
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 09) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 16) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 23) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 30) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 07) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 14) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 21) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 28) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 04) (TimeOfDay 09 00 00)
                   ]
    specify "Weekly until December 24, 1997" $ do
      --
      --  DTSTART;TZID=America/New_York:19970902T090000
      --  RRULE:FREQ=WEEKLY;UNTIL=19971224T000000Z
      --
      --  ==> (1997 9:00 AM EDT) September 2,9,16,23,30;
      --                         October 7,14,21
      --      (1997 9:00 AM EST) October 28;
      --                         November 4,11,18,25;
      --                         December 2,9,16,23
      --
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule = (rRule Weekly) {rRuleUntilCount = Until (LocalTime (fromGregorian 1997 12 24) (TimeOfDay 00 00 00))}
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 09) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 16) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 23) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 30) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 07) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 14) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 21) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 28) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 04) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 11) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 18) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 25) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 09) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 16) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 23) (TimeOfDay 09 00 00)
                   ]
    specify "Every other week - forever" $ do
      --
      --  DTSTART;TZID=America/New_York:19970902T090000
      --  RRULE:FREQ=WEEKLY;INTERVAL=2;WKST=SU
      --
      --  ==> (1997 9:00 AM EDT) September 2,16,30;
      --                         October 14
      --      (1997 9:00 AM EST) October 28;
      --                         November 11,25;
      --                         December 9,23
      --      (1998 9:00 AM EST) January 6,20;
      --                         February 3, 17
      --      ...
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule = (rRule Weekly) {rRuleInterval = Interval 2, rRuleWeekStart = Sunday}
          limit = LocalTime (fromGregorian 1998 02 17) (TimeOfDay 09 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 16) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 30) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 14) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 28) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 11) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 25) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 09) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 23) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 06) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 20) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 02 03) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 02 17) (TimeOfDay 09 00 00)
                   ]
    specify "Weekly on Tuesday and Thursday for five weeks" $ do
      --
      --  DTSTART;TZID=America/New_York:19970902T090000
      --  RRULE:FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH
      --
      --  or
      --
      --  RRULE:FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH
      --
      --  ==> (1997 9:00 AM EDT) September 2,4,9,11,16,18,23,25,30;
      --                         October 2
      --
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule1 =
            (rRule Weekly)
              { rRuleUntilCount = Until (LocalTime (fromGregorian 1997 10 07) (TimeOfDay 00 00 00)),
                rRuleWeekStart = Sunday,
                rRuleByDay = [Every Tuesday, Every Thursday]
              }
          rule2 =
            (rRule Weekly)
              { rRuleUntilCount = Count 10,
                rRuleWeekStart = Sunday,
                rRuleByDay = [Every Tuesday, Every Thursday]
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
      let res1 = rruleDateTimeOccurrencesUntil dtstart rule1 limit
      let res2 = rruleDateTimeOccurrencesUntil dtstart rule2 limit
      res1
        `shouldBe` [ LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 04) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 09) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 11) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 16) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 18) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 23) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 25) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 30) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 02) (TimeOfDay 09 00 00)
                   ]
      res2 `shouldBe` res1
    specify "Every other week on Monday, Wednesday, and Friday until December 24, 1997, starting on Monday, September 1, 1997" $ do
      --
      --  DTSTART;TZID=America/New_York:19970901T090000
      --  RRULE:FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;WKST=SU;
      --   BYDAY=MO,WE,FR
      --
      --  ==> (1997 9:00 AM EDT) September 1,3,5,15,17,19,29;
      --                         October 1,3,13,15,17
      --      (1997 9:00 AM EST) October 27,29,31;
      --                         November 10,12,14,24,26,28;
      --
      --                         December 8,10,12,22
      --
      let dtstart = LocalTime (fromGregorian 1997 09 01) (TimeOfDay 09 00 00)
          rule =
            (rRule Weekly)
              { rRuleInterval = Interval 2,
                rRuleUntilCount = Until (LocalTime (fromGregorian 1997 12 24) (TimeOfDay 00 00 00)),
                rRuleWeekStart = Sunday,
                rRuleByDay =
                  [ Every Monday,
                    Every Wednesday,
                    Every Friday
                  ]
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 01) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 03) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 05) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 15) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 17) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 19) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 29) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 01) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 03) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 13) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 15) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 17) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 27) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 29) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 31) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 12) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 14) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 24) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 26) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 28) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 08) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 12) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 22) (TimeOfDay 09 00 00)
                   ]
    specify "Every other week on Tuesday and Thursday, for 8 occurrences" $ do
      --
      --  DTSTART;TZID=America/New_York:19970902T090000
      --  RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH
      --
      --  ==> (1997 9:00 AM EDT) September 2,4,16,18,30;
      --                         October 2,14,16
      --
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule =
            (rRule Weekly)
              { rRuleInterval = Interval 2,
                rRuleUntilCount = Count 8,
                rRuleWeekStart = Sunday,
                rRuleByDay = [Every Tuesday, Every Thursday]
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 04) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 16) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 18) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 30) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 14) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 16) (TimeOfDay 09 00 00)
                   ]
    specify "Monthly on the first Friday for 10 occurrences" $ do
      --
      --  DTSTART;TZID=America/New_York:19970905T090000
      --  RRULE:FREQ=MONTHLY;COUNT=10;BYDAY=1FR
      --
      --  ==> (1997 9:00 AM EDT) September 5;October 3
      --      (1997 9:00 AM EST) November 7;December 5
      --      (1998 9:00 AM EST) January 2;February 6;March 6;April 3
      --      (1998 9:00 AM EDT) May 1;June 5
      --
      let dtstart = LocalTime (fromGregorian 1997 09 05) (TimeOfDay 09 00 00)
          rule =
            (rRule Monthly)
              { rRuleUntilCount = Count 10,
                rRuleByDay = [Specific 1 Friday]
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 05) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 03) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 07) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 05) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 02 06) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 03 06) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 04 03) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 05 01) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 06 05) (TimeOfDay 09 00 00)
                   ]
    specify "Monthly on the first Friday until December 24, 1997" $ do
      --
      --  DTSTART;TZID=America/New_York:19970905T090000
      --  RRULE:FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR
      --
      --  ==> (1997 9:00 AM EDT) September 5; October 3
      --      (1997 9:00 AM EST) November 7; December 5
      --
      let dtstart = LocalTime (fromGregorian 1997 09 05) (TimeOfDay 09 00 00)
          rule =
            (rRule Monthly)
              { rRuleUntilCount = Until (LocalTime (fromGregorian 1997 12 24) (TimeOfDay 00 00 00)),
                rRuleByDay = [Specific 1 Friday]
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 05) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 03) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 07) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 05) (TimeOfDay 09 00 00)
                   ]
    specify "Every other month on the first and last Sunday of the month for 10 occurrences" $ do
      --
      --  DTSTART;TZID=America/New_York:19970907T090000
      --  RRULE:FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU
      --
      --  ==> (1997 9:00 AM EDT) September 7,28
      --      (1997 9:00 AM EST) November 2,30
      --      (1998 9:00 AM EST) January 4,25;March 1,29
      --      (1998 9:00 AM EDT) May 3,31
      --
      let dtstart = LocalTime (fromGregorian 1997 09 07) (TimeOfDay 09 00 00)
          rule =
            (rRule Monthly)
              { rRuleInterval = Interval 2,
                rRuleUntilCount = Count 10,
                rRuleByDay = [Specific 1 Sunday, Specific (-1) Sunday]
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 07) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 28) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 30) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 04) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 25) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 03 01) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 03 29) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 05 03) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 05 31) (TimeOfDay 09 00 00)
                   ]
    specify "Monthly on the second-to-last Monday of the month for 6 months" $ do
      --
      --  DTSTART;TZID=America/New_York:19970922T090000
      --  RRULE:FREQ=MONTHLY;COUNT=6;BYDAY=-2MO
      --
      --  ==> (1997 9:00 AM EDT) September 22;October 20
      --      (1997 9:00 AM EST) November 17;December 22
      --      (1998 9:00 AM EST) January 19;February 16
      --
      let dtstart = LocalTime (fromGregorian 1997 09 22) (TimeOfDay 09 00 00)
          rule =
            (rRule Monthly)
              { rRuleUntilCount = Count 6,
                rRuleByDay = [Specific (-2) Monday]
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 22) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 20) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 17) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 22) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 19) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 02 16) (TimeOfDay 09 00 00)
                   ]
    specify "Monthly on the third-to-the-last day of the month, forever" $ do
      --
      --  DTSTART;TZID=America/New_York:19970928T090000
      --  RRULE:FREQ=MONTHLY;BYMONTHDAY=-3
      --
      --  ==> (1997 9:00 AM EDT) September 28
      --      (1997 9:00 AM EST) October 29;November 28;December 29
      --      (1998 9:00 AM EST) January 29;February 26
      --      ...
      --
      let dtstart = LocalTime (fromGregorian 1997 09 28) (TimeOfDay 09 00 00)
          rule = (rRule Monthly) {rRuleByMonthDay = [MonthDay (-3)]}
          limit = LocalTime (fromGregorian 1998 02 27) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 28) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 29) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 28) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 29) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 29) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 02 26) (TimeOfDay 09 00 00)
                   ]
    specify "Monthly on the 2nd and 15th of the month for 10 occurrences" $ do
      --
      --  DTSTART;TZID=America/New_York:19970902T090000
      --  RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15
      --
      --  ==> (1997 9:00 AM EDT) September 2,15;October 2,15
      --      (1997 9:00 AM EST) November 2,15;December 2,15
      --      (1998 9:00 AM EST) January 2,15
      --
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule = (rRule Monthly) {rRuleUntilCount = Count 10, rRuleByMonthDay = [MonthDay 2, MonthDay 15]}
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 15) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 15) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 15) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 15) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 15) (TimeOfDay 09 00 00)
                   ]
    specify "Monthly on the first and last day of the month for 10 occurrences" $ do
      --
      --  DTSTART;TZID=America/New_York:19970930T090000
      --  RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1
      --
      --  ==> (1997 9:00 AM EDT) September 30;October 1
      --      (1997 9:00 AM EST) October 31;November 1,30;December 1,31
      --      (1998 9:00 AM EST) January 1,31;February 1
      --
      let dtstart = LocalTime (fromGregorian 1997 09 30) (TimeOfDay 09 00 00)
          rule =
            (rRule Monthly)
              { rRuleUntilCount = Count 10,
                rRuleByMonthDay = [MonthDay 1, MonthDay (-1)]
              }
          limit = LocalTime (fromGregorian 1998 02 02) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 30) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 01) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 31) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 01) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 30) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 01) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 31) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 01) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 31) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 02 01) (TimeOfDay 09 00 00)
                   ]
    specify "Every 18 months on the 10th thru 15th of the month for 10 occurrences" $ do
      --
      --  DTSTART;TZID=America/New_York:19970910T090000
      --  RRULE:FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,
      --   13,14,15
      --
      --  ==> (1997 9:00 AM EDT) September 10,11,12,13,14,15
      --      (1999 9:00 AM EST) March 10,11,12,13
      --
      let dtstart = LocalTime (fromGregorian 1997 09 10) (TimeOfDay 09 00 00)
          rule =
            (rRule Monthly)
              { rRuleInterval = Interval 18,
                rRuleUntilCount = Count 10,
                rRuleByMonthDay =
                  [ MonthDay 10,
                    MonthDay 11,
                    MonthDay 12,
                    MonthDay 13,
                    MonthDay 14,
                    MonthDay 15
                  ]
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 11) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 12) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 13) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 14) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 15) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 03 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 03 11) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 03 12) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 03 13) (TimeOfDay 09 00 00)
                   ]
    specify "Every Tuesday, every other month" $ do
      --
      --  DTSTART;TZID=America/New_York:19970902T090000
      --  RRULE:FREQ=MONTHLY;INTERVAL=2;BYDAY=TU
      --
      --  ==> (1997 9:00 AM EDT) September 2,9,16,23,30
      --      (1997 9:00 AM EST) November 4,11,18,25
      --      (1998 9:00 AM EST) January 6,13,20,27;March 3,10,17,24,31
      --      ...
      --
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule =
            (rRule Monthly)
              { rRuleInterval = Interval 2,
                rRuleByDay = [Every Tuesday]
              }
          limit = LocalTime (fromGregorian 1998 04 01) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 09) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 16) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 23) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 30) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 04) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 11) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 18) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 25) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 06) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 13) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 20) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 27) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 03 03) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 03 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 03 17) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 03 24) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 03 31) (TimeOfDay 09 00 00)
                   ]
    specify "Yearly in June and July for 10 occurrences" $ do
      --
      --  DTSTART;TZID=America/New_York:19970610T090000
      --  RRULE:FREQ=YEARLY;COUNT=10;BYMONTH=6,7
      --
      --  ==> (1997 9:00 AM EDT) June 10;July 10
      --      (1998 9:00 AM EDT) June 10;July 10
      --      (1999 9:00 AM EDT) June 10;July 10
      --      (2000 9:00 AM EDT) June 10;July 10
      --      (2001 9:00 AM EDT) June 10;July 10
      --
      --    Note: Since none of the BYDAY, BYMONTHDAY, or BYYEARDAY
      --    components are specified, the day is gotten from "DTSTART".
      --
      let dtstart = LocalTime (fromGregorian 1997 06 10) (TimeOfDay 09 00 00)
          rule =
            (rRule Yearly)
              { rRuleUntilCount = Count 10,
                rRuleByMonth = [June, July]
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2020 00 00) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 06 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 07 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 06 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 07 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 06 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 07 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2000 06 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2000 07 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2001 06 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2001 07 10) (TimeOfDay 09 00 00)
                   ]
    specify "Every other year on January, February, and March for 10 occurrences" $ do
      --
      --  DTSTART;TZID=America/New_York:19970310T090000
      --  RRULE:FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3
      --
      --  ==> (1997 9:00 AM EST) March 10
      --      (1999 9:00 AM EST) January 10;February 10;March 10
      --      (2001 9:00 AM EST) January 10;February 10;March 10
      --      (2003 9:00 AM EST) January 10;February 10;March 10
      --
      let dtstart = LocalTime (fromGregorian 1997 03 10) (TimeOfDay 09 00 00)
          rule =
            (rRule Yearly)
              { rRuleInterval = Interval 2,
                rRuleUntilCount = Count 10,
                rRuleByMonth = [January, February, March]
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2020 00 00) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 03 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 01 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 02 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 03 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2001 01 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2001 02 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2001 03 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2003 01 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2003 02 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2003 03 10) (TimeOfDay 09 00 00)
                   ]
    specify "Every third year on the 1st, 100th, and 200th day for 10 occurrences" $ do
      --
      --  DTSTART;TZID=America/New_York:19970101T090000
      --  RRULE:FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200
      --
      --  ==> (1997 9:00 AM EST) January 1
      --      (1997 9:00 AM EDT) April 10;July 19
      --      (2000 9:00 AM EST) January 1
      --      (2000 9:00 AM EDT) April 9;July 18
      --      (2003 9:00 AM EST) January 1
      --      (2003 9:00 AM EDT) April 10;July 19
      --      (2006 9:00 AM EST) January 1
      --
      let dtstart = LocalTime (fromGregorian 1997 01 01) (TimeOfDay 09 00 00)
          rule =
            (rRule Yearly)
              { rRuleInterval = Interval 3,
                rRuleUntilCount = Count 10,
                rRuleByYearDay =
                  [ YearDay 1,
                    YearDay 100,
                    YearDay 200
                  ]
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2020 00 00) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 01 01) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 04 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 07 19) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2000 01 01) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2000 04 09) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2000 07 18) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2003 01 01) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2003 04 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2003 07 19) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2006 01 01) (TimeOfDay 09 00 00)
                   ]
    specify "Every 20th Monday of the year, forever" $ do
      --
      --  DTSTART;TZID=America/New_York:19970519T090000
      --  RRULE:FREQ=YEARLY;BYDAY=20MO
      --
      --  ==> (1997 9:00 AM EDT) May 19
      --      (1998 9:00 AM EDT) May 18
      --      (1999 9:00 AM EDT) May 17
      --      ...
      --
      --
      let dtstart = LocalTime (fromGregorian 1997 05 19) (TimeOfDay 09 00 00)
          rule =
            (rRule Yearly)
              { rRuleByDay = [Specific 20 Monday]
              }
          limit = LocalTime (fromGregorian 1999 05 18) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 05 19) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 05 18) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 05 17) (TimeOfDay 09 00 00)
                   ]
    specify "Monday of week number 20 (where the default start of the week is Monday), forever" $ do
      --
      --  DTSTART;TZID=America/New_York:19970512T090000
      --  RRULE:FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO
      --
      --  ==> (1997 9:00 AM EDT) May 12
      --      (1998 9:00 AM EDT) May 11
      --      (1999 9:00 AM EDT) May 17
      --      ...
      --
      let dtstart = LocalTime (fromGregorian 1997 05 12) (TimeOfDay 09 00 00)
          rule =
            (rRule Yearly)
              { rRuleByWeekNo = [WeekNo 20],
                rRuleByDay = [Every Monday]
              }
          limit = LocalTime (fromGregorian 1999 05 18) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 05 12) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 05 11) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 05 17) (TimeOfDay 09 00 00)
                   ]
    specify "Every Thursday in March, forever" $ do
      --
      --  DTSTART;TZID=America/New_York:19970313T090000
      --  RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=TH
      --
      --  ==> (1997 9:00 AM EST) March 13,20,27
      --      (1998 9:00 AM EST) March 5,12,19,26
      --      (1999 9:00 AM EST) March 4,11,18,25
      --      ...
      --
      let dtstart = LocalTime (fromGregorian 1997 03 13) (TimeOfDay 09 00 00)
          rule =
            (rRule Yearly)
              { rRuleByMonth = [March],
                rRuleByDay = [Every Thursday]
              }
          limit = LocalTime (fromGregorian 1999 03 26) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 03 13) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 03 20) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 03 27) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 03 05) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 03 12) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 03 19) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 03 26) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 03 04) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 03 11) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 03 18) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 03 25) (TimeOfDay 09 00 00)
                   ]
    specify "Every Thursday, but only during June, July, and August, forever" $ do
      --
      --  DTSTART;TZID=America/New_York:19970605T090000
      --  RRULE:FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8
      --
      --  ==> (1997 9:00 AM EDT) June 5,12,19,26;July 3,10,17,24,31;
      --                         August 7,14,21,28
      --      (1998 9:00 AM EDT) June 4,11,18,25;July 2,9,16,23,30;
      --                         August 6,13,20,27
      --      (1999 9:00 AM EDT) June 3,10,17,24;July 1,8,15,22,29;
      --                         August 5,12,19,26
      --      ...
      --
      let dtstart = LocalTime (fromGregorian 1997 06 05) (TimeOfDay 09 00 00)
          rule =
            (rRule Yearly)
              { rRuleByDay = [Every Thursday],
                rRuleByMonth = [June, July, August]
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 00 00) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 06 05) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 06 12) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 06 19) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 06 26) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 07 03) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 07 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 07 17) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 07 24) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 07 31) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 08 07) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 08 14) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 08 21) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 08 28) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 06 04) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 06 11) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 06 18) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 06 25) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 07 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 07 09) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 07 16) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 07 23) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 07 30) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 08 06) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 08 13) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 08 20) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 08 27) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 06 03) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 06 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 06 17) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 06 24) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 07 01) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 07 08) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 07 15) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 07 22) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 07 29) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 08 05) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 08 12) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 08 19) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 08 26) (TimeOfDay 09 00 00)
                   ]
    specify "Every Friday the 13th, forever" $ do
      --
      --  DTSTART;TZID=America/New_York:19970902T090000
      --  EXDATE;TZID=America/New_York:19970902T090000
      --  RRULE:FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13
      --
      --  ==> (1998 9:00 AM EST) February 13;March 13;November 13
      --      (1999 9:00 AM EDT) August 13
      --      (2000 9:00 AM EDT) October 13
      --      ...
      --
      --
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule =
            (rRule Monthly)
              { rRuleByDay = [Every Friday],
                rRuleByMonthDay = [MonthDay 13]
              }
          limit = LocalTime (fromGregorian 2000 10 14) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 02 13) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 03 13) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 11 13) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1999 08 13) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2000 10 13) (TimeOfDay 09 00 00)
                   ]
    specify "The first Saturday that follows the first Sunday of the month, forever" $ do
      --
      --  DTSTART;TZID=America/New_York:19970913T090000
      --  RRULE:FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13
      --
      --  ==> (1997 9:00 AM EDT) September 13;October 11
      --      (1997 9:00 AM EST) November 8;December 13
      --      (1998 9:00 AM EST) January 10;February 7;March 7
      --      (1998 9:00 AM EDT) April 11;May 9;June 13...
      --      ...
      --
      let dtstart = LocalTime (fromGregorian 1997 09 13) (TimeOfDay 09 00 00)
          rule =
            (rRule Monthly)
              { rRuleByDay = [Every Saturday],
                rRuleByMonthDay =
                  [ MonthDay 7,
                    MonthDay 8,
                    MonthDay 9,
                    MonthDay 9,
                    MonthDay 10,
                    MonthDay 11,
                    MonthDay 12,
                    MonthDay 13
                  ]
              }
          limit = LocalTime (fromGregorian 1998 06 14) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 13) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 11) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 08) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 13) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 02 07) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 03 07) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 04 11) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 05 09) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 06 13) (TimeOfDay 09 00 00)
                   ]
    specify "Every 4 years, the first Tuesday after a Monday in November, forever (U.S. Presidential Election day)" $ do
      --
      --  DTSTART;TZID=America/New_York:19961105T090000
      --  RRULE:FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;
      --   BYMONTHDAY=2,3,4,5,6,7,8
      --
      --   ==> (1996 9:00 AM EST) November 5
      --       (2000 9:00 AM EST) November 7
      --       (2004 9:00 AM EST) November 2
      --       ...
      --
      let dtstart = LocalTime (fromGregorian 1996 11 05) (TimeOfDay 09 00 00)
          rule =
            (rRule Yearly)
              { rRuleInterval = Interval 4,
                rRuleByMonth = [November],
                rRuleByDay = [Every Tuesday],
                rRuleByMonthDay =
                  [ MonthDay 2,
                    MonthDay 3,
                    MonthDay 4,
                    MonthDay 5,
                    MonthDay 6,
                    MonthDay 7,
                    MonthDay 8
                  ]
              }
          limit = LocalTime (fromGregorian 2004 11 03) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1996 11 05) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2000 11 07) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2004 11 02) (TimeOfDay 09 00 00)
                   ]
    specify "The third instance into the month of one of Tuesday, Wednesday, or Thursday, for the next 3 months" $ do
      --
      --  DTSTART;TZID=America/New_York:19970904T090000
      --  RRULE:FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3
      --
      --  ==> (1997 9:00 AM EDT) September 4;October 7
      --      (1997 9:00 AM EST) November 6
      --
      let dtstart = LocalTime (fromGregorian 1997 09 04) (TimeOfDay 09 00 00)
          rule =
            (rRule Monthly)
              { rRuleUntilCount = Count 3,
                rRuleByDay =
                  [ Every Tuesday,
                    Every Wednesday,
                    Every Thursday
                  ],
                rRuleBySetPos = [SetPos 3]
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 00 00) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 04) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 07) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 06) (TimeOfDay 09 00 00)
                   ]
    specify "The second-to-last weekday of the month" $ do
      --
      --  DTSTART;TZID=America/New_York:19970929T090000
      --  RRULE:FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2
      --
      --  ==> (1997 9:00 AM EDT) September 29
      --      (1997 9:00 AM EST) October 30;November 27;December 30
      --      (1998 9:00 AM EST) January 29;February 26;March 30
      --      ...
      --
      let dtstart = LocalTime (fromGregorian 1997 09 29) (TimeOfDay 09 00 00)
          rule =
            (rRule Monthly)
              { rRuleByDay =
                  [ Every Monday,
                    Every Tuesday,
                    Every Wednesday,
                    Every Thursday,
                    Every Friday
                  ],
                rRuleBySetPos = [SetPos (-2)]
              }
          limit = LocalTime (fromGregorian 1998 04 01) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 29) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 10 30) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 11 27) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 12 30) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 01 29) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 02 26) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1998 03 30) (TimeOfDay 09 00 00)
                   ]
    xspecify "Every 3 hours from 9:00 AM to 5:00 PM on a specific day" $ do
      --
      --  DTSTART;TZID=America/New_York:19970902T090000
      --  RRULE:FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z
      --
      --  ==> (September 2, 1997 EDT) 09:00,12:00,15:00
      --
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule =
            (rRule Hourly)
              { rRuleInterval = Interval 3,
                rRuleUntilCount = Until (LocalTime (fromGregorian 1997 09 02) (TimeOfDay 00 00 00))
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 00 00) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 02) (TimeOfDay 12 00 00),
                     LocalTime (fromGregorian 1997 09 02) (TimeOfDay 15 00 00)
                   ]
    xspecify "Every 15 minutes for 6 occurrences" $ do
      --
      --  DTSTART;TZID=America/New_York:19970902T090000
      --  RRULE:FREQ=MINUTELY;INTERVAL=15;COUNT=6
      --
      --  ==> (September 2, 1997 EDT) 09:00,09:15,09:30,09:45,10:00,10:15
      --
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule =
            (rRule Minutely)
              { rRuleInterval = Interval 15,
                rRuleUntilCount = Count 6
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 00 00) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 15 00),
                     LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 30 00),
                     LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 45 00),
                     LocalTime (fromGregorian 1997 09 02) (TimeOfDay 10 00 00),
                     LocalTime (fromGregorian 1997 09 02) (TimeOfDay 10 15 00)
                   ]
    xspecify "Every hour and a half for 4 occurrences" $ do
      --
      --  DTSTART;TZID=America/New_York:19970902T090000
      --  RRULE:FREQ=MINUTELY;INTERVAL=90;COUNT=4
      --
      --  ==> (September 2, 1997 EDT) 09:00,10:30;12:00;13:30
      --
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule =
            (rRule Minutely)
              { rRuleInterval = Interval 90,
                rRuleUntilCount = Count 4
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 00 00) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 09 02) (TimeOfDay 10 30 00),
                     LocalTime (fromGregorian 1997 09 02) (TimeOfDay 12 00 00),
                     LocalTime (fromGregorian 1997 09 02) (TimeOfDay 13 30 00)
                   ]
    xspecify "Every 20 minutes from 9:00 AM to 4:40 PM every day" $ do
      --
      --  DTSTART;TZID=America/New_York:19970902T090000
      --  RRULE:FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40
      --  or
      --  RRULE:FREQ=MINUTELY;INTERVAL=20;BYHOUR=9,10,11,12,13,14,15,16
      --
      --  ==> (September 2, 1997 EDT) 9:00,9:20,9:40,10:00,10:20,
      --                              ... 16:00,16:20,16:40
      --      (September 3, 1997 EDT) 9:00,9:20,9:40,10:00,10:20,
      --                              ... 16:00,16:20,16:40
      --      ...
      --
      let dtstart = LocalTime (fromGregorian 1997 09 02) (TimeOfDay 09 00 00)
          rule1 =
            (rRule Daily)
              { rRuleByHour =
                  [ Hour 9,
                    Hour 10,
                    Hour 11,
                    Hour 12,
                    Hour 13,
                    Hour 14,
                    Hour 15,
                    Hour 16
                  ],
                rRuleByMinute =
                  [ Minute 0,
                    Minute 20,
                    Minute 40
                  ]
              }
          rule2 =
            (rRule Minutely)
              { rRuleInterval = Interval 20,
                rRuleByHour =
                  [ Hour 9,
                    Hour 10,
                    Hour 11,
                    Hour 12,
                    Hour 13,
                    Hour 14,
                    Hour 15,
                    Hour 16
                  ]
              }
          limit = LocalTime (fromGregorian 1997 09 03) (TimeOfDay 17 00 00)
      let res1 = rruleDateTimeOccurrencesUntil dtstart rule1 limit
      let res2 = rruleDateTimeOccurrencesUntil dtstart rule2 limit
      res1
        `shouldBe` S.fromList
          ( do
              dn <- [2, 3]
              h <- [9 .. 16]
              m <- [00, 20, 40]
              pure (LocalTime (fromGregorian 1997 09 dn) (TimeOfDay h m 00))
          )
      res2 `shouldBe` res1
    specify "An example where the days generated makes a difference because of WKST" $ do
      --
      --  DTSTART;TZID=America/New_York:19970805T090000
      --  RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO
      --
      --  ==> (1997 EDT) August 5,10,19,24
      --
      let dtstart = LocalTime (fromGregorian 1997 08 05) (TimeOfDay 09 00 00)
          rule =
            (rRule Weekly)
              { rRuleInterval = Interval 2,
                rRuleUntilCount = Count 4,
                rRuleByDay = [Every Tuesday, Every Sunday],
                rRuleWeekStart = Monday
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 00 00) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 08 05) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 08 10) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 08 19) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 08 24) (TimeOfDay 09 00 00)
                   ]
    specify "changing only WKST from MO to SU, yields different results.." $ do
      --
      --  DTSTART;TZID=America/New_York:19970805T090000
      --  RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU
      --
      --  ==> (1997 EDT) August 5,17,19,31
      --
      let dtstart = LocalTime (fromGregorian 1997 08 05) (TimeOfDay 09 00 00)
          rule =
            (rRule Weekly)
              { rRuleInterval = Interval 2,
                rRuleUntilCount = Count 4,
                rRuleByDay = [Every Tuesday, Every Sunday],
                rRuleWeekStart = Sunday
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2000 00 00) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 1997 08 05) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 08 17) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 08 19) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 1997 08 31) (TimeOfDay 09 00 00)
                   ]
    specify "An example where an invalid date (i.e., February 30) is ignored" $ do
      --  DTSTART;TZID=America/New_York:20070115T090000
      --  RRULE:FREQ=MONTHLY;BYMONTHDAY=15,30;COUNT=5
      --
      --  ==> (2007 EST) January 15,30
      --      (2007 EST) February 15
      --      (2007 EDT) March 15,30
      let dtstart = LocalTime (fromGregorian 2007 01 15) (TimeOfDay 09 00 00)
          rule =
            (rRule Monthly)
              { rRuleByMonthDay = [MonthDay 15, MonthDay 30],
                rRuleUntilCount = Count 5
              }
          -- Limit: the set is finite so the limit will just be some point beyond the end
          limit = LocalTime (fromGregorian 2020 00 00) (TimeOfDay 00 00 00)
      rruleDateTimeOccurrencesUntil dtstart rule limit
        `shouldBe` [ LocalTime (fromGregorian 2007 01 15) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2007 01 30) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2007 02 15) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2007 03 15) (TimeOfDay 09 00 00),
                     LocalTime (fromGregorian 2007 03 30) (TimeOfDay 09 00 00)
                   ]
