module Smos.Calendar.Import.WeekDate where

import Control.Exception
import Data.Time
import Data.Validity.Containers ()
import Data.Validity.Time ()

nextWeek :: DayOfWeek -> Integer -> Word -> (Integer, Word)
nextWeek ws y wn
  | wn < 52 = (y, wn + 1)
  | wn == 52 =
    -- From https://en.wikipedia.org/wiki/ISO_week_date#Last_week
    -- [The last week] has 28 December in it.

    let dayInLastWeek = fromGregorian y 12 28
        (_, wnOfLastWeek, _) = toWeekDateWithStart ws dayInLastWeek
     in if wnOfLastWeek <= 52
          then-- There are 52 weeks this year, so we go to the next year
            (y + 1, 1)
          else-- There are 53 weeks in this year, so we go to to that week
            (y, 53)
  | otherwise = (y + 1, 1)

weeksIntoTheFutureStartingFrom :: DayOfWeek -> Integer -> Word -> [(Integer, Word)]
weeksIntoTheFutureStartingFrom ws = go
  where
    go y wn =
      let (y', wn') = nextWeek ws y wn
       in (y, wn) : go y' wn'

-- | Calculate the year, week number and weekday of a day, given a day on which the week starts
--
-- The BYWEEKNO rule part specifies a COMMA-separated list of
-- ordinals specifying weeks of the year.  Valid values are 1 to 53
-- or -53 to -1.  This corresponds to weeks according to week
-- numbering as defined in [ISO.8601.2004].  A week is defined as a
-- seven day period, starting on the day of the week defined to be
-- the week start (see WKST).  Week number one of the calendar year
-- is the first week that contains at least four (4) days in that
-- calendar year.
--
--    Note: Assuming a Monday week start, week 53 can only occur when
--    Thursday is January 1 or if it is a leap year and Wednesday is
--    January 1.
--
-- This means that in 2015, when Jan 1st was a thursday:
-- - with a week start of Monday, the first week started on 29 dec 2014
-- - with a week start of Sunday, the first week started on 4 jan 2015
toWeekDateWithStart :: DayOfWeek -> Day -> (Integer, Word, DayOfWeek)
toWeekDateWithStart ws d =
  let dow = dayOfWeek d
      (year, _, _) = toGregorian d
      firstDayOfTheFirstWsWeekThisYear = firstDayOfTheFirstWsWeekOf ws year
      firstDayOfTheFirstWsWeekNextYear = firstDayOfTheFirstWsWeekOf ws (year + 1)
      (wsWeekYear, wsWeekNo)
        | d < firstDayOfTheFirstWsWeekThisYear = (year - 1, 53) -- TODO leap year to see if it's 53 or 52
        | d >= firstDayOfTheFirstWsWeekNextYear = (year + 1, 1)
        | otherwise = (year, fromInteger $ (diffDays d firstDayOfTheFirstWsWeekThisYear `quot` 7) + 1)
   in (wsWeekYear, wsWeekNo, dow)

daysInYear :: Integer -> Int
daysInYear y = if isLeapYear y then 366 else 365

fromWeekDateWithStart :: DayOfWeek -> Integer -> Word -> DayOfWeek -> Maybe Day
fromWeekDateWithStart ws year w dow =
  Just $
    let firstD = firstDayOfTheFirstWsWeekOf ws year
        weekOffset = positiveMod 7 $ fromEnum dow - fromEnum ws
     in addDays (fromIntegral $ 7 * (w -1) + fromIntegral weekOffset) firstD

-- We want to know whethere the first 'ws' occurs in the first week of
-- this year or in the last week of last year
-- Example: If the first 'ws' occurs on jan 1st then it's easy because
-- then it's definitely the first week of this year beacuse then all 7
-- days of that week are in this year.
-- To make sure that four of the days in the week that started on the
-- 'ws' week that contains jan 1, the 'ws' of that week must have
-- occurred on or after the third-to-last day of the previous year.
-- In that case the first week started in the previous year.
-- If that 'ws' occurred before the third-to-last day of the previous year,
-- then the first week started in the current year.
-- The third-to-last day of the previous year is always 29 dec, even in
-- leap years
--
-- For example, if Jan 1st is a thursday then the first monday-week of the year started this year
-- but if Jan 1st is a Wednesday then the first monday-week of the year started last year
--
-- If the 'firstDayOfTheWSWeekThatContainsJan1st' is on or after dec 29
-- then it is the first day of the first ws week otherwise, the first
-- week starts a week later.
firstDayOfTheFirstWsWeekOf :: DayOfWeek -> Integer -> Day
firstDayOfTheFirstWsWeekOf ws year =
  let firstDayOfTheWSWeekThatContainsJan1stForD = firstDayOfTheWSWeekThatContainsJan1st ws year
   in assert (dayOfWeek firstDayOfTheWSWeekThatContainsJan1stForD == ws) $
        if firstDayOfTheWSWeekThatContainsJan1stForD >= fromGregorian (year - 1) 12 29
          then firstDayOfTheWSWeekThatContainsJan1stForD
          else addDays 7 firstDayOfTheWSWeekThatContainsJan1stForD

-- | The first 'ws' day of the week that contains jan 1st
--
-- Example 1: If Jan 1st is a thursday and the week starts on monday
-- then dec 29 is the first day of the 'monday'-week that contains jan 1st
-- so we have to subtract 3, which is positiveMod 7 (Thursday (4) - Monday (1))
--
-- Example 2: If Jan 1st is a thursday and the week starts on saturday
-- then then dec 27 is the firstay day of the 'monday'-week start contains jan 1st
-- so we have to subtract 5, which is positiveMod 7 (Thursday (4) - Saturday (6))
firstDayOfTheWSWeekThatContainsJan1st :: DayOfWeek -> Integer -> Day
firstDayOfTheWSWeekThatContainsJan1st ws year =
  let firstDayOfTheYear = fromGregorian year 1 1
      dowFirstDayOfTheYear = dayOfWeek firstDayOfTheYear
   in addDays (negate $ positiveMod 7 $ fromIntegral $ fromEnum dowFirstDayOfTheYear - fromEnum ws) firstDayOfTheYear

positiveMod :: Integral i => i -> i -> i
positiveMod r n =
  let m = n `mod` r
   in if m < 0 then m + r else m
