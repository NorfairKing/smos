{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.RecurrenceRule where

import Control.Monad
import Data.Aeson.Types (Pair)
import Data.Fixed
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import Data.Time.Calendar.MonthDay
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Time ()
import Data.Yaml
import GHC.Generics (Generic)
import Safe
import Smos.Calendar.Import.WeekDate
import Smos.Data
import YamlParse.Applicative

-- | A recurrence rule
--
-- = Definition
-- Recurrence rules are defined in section 3.3.10: "Recurrence rule"
-- https://tools.ietf.org/html/rfc5545#section-3.3.10
--
-- There is more info about how they are used in section 3.8.5.3: "Recurrence rules"
-- https://tools.ietf.org/html/rfc5545#section-3.8.5.3
--
-- Do not even _think_ about modifying this code unless you have read these
-- parts of the spec.
--
-- = From section 3.3.10
--
-- Purpose: This value type is used to identify properties that contain a
-- recurrence rule specification.
--
-- Recurrence rules may generate recurrence instances with an invalid date
-- (e.g., February 30) or nonexistent local time (e.g., 1:30 AM on a day where
-- the local time is moved forward by an hour at 1:00 AM).  Such recurrence
-- instances MUST be ignored and MUST NOT be counted as part of the recurrence
-- set.
--
-- Information, not contained in the rule, necessary to determine the various
-- recurrence instance start time and dates are derived from the Start Time
-- ("DTSTART") component attribute.  For example, "FREQ=YEARLY;BYMONTH=1"
-- doesn't specify a specific day within the month or a time.  This information
-- would be the same as what is specified for "DTSTART".
--
-- BYxxx rule parts modify the recurrence in some manner.  BYxxx rule parts for
-- a period of time that is the same or greater than the frequency generally
-- reduce or limit the number of occurrences of the recurrence generated.  For
-- example, "FREQ=DAILY;BYMONTH=1" reduces the number of recurrence instances
-- from all days (if BYMONTH rule part is not present) to all days in January.
-- BYxxx rule parts for a period of time less than the frequency generally
-- increase or expand the number of occurrences of the recurrence.  For
-- example, "FREQ=YEARLY;BYMONTH=1,2" increases the number of days within the
-- yearly recurrence set from 1 (if BYMONTH rule part is not present) to 2.
--
-- If multiple BYxxx rule parts are specified, then after evaluating the
-- specified FREQ and INTERVAL rule parts, the BYxxx rule parts are applied to
-- the current set of evaluated occurrences in the following order: BYMONTH,
-- BYWEEKNO, BYYEARDAY, BYMONTHDAY, BYDAY, BYHOUR, BYMINUTE, BYSECOND and
-- BYSETPOS; then COUNT and UNTIL are evaluated.
--
-- The table below summarizes the dependency of BYxxx rule part expand or limit
-- behavior on the FREQ rule part value.
--
-- The term "N/A" means that the corresponding BYxxx rule part MUST NOT be used
-- with the corresponding FREQ value.
--
-- BYDAY has some special behavior depending on the FREQ value and this is
-- described in separate notes below the table.
--
-- > +----------+--------+--------+-------+-------+------+-------+------+
-- > |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
-- > +----------+--------+--------+-------+-------+------+-------+------+
-- > |BYMONTH   |Limit   |Limit   |Limit  |Limit  |Limit |Limit  |Expand|
-- > +----------+--------+--------+-------+-------+------+-------+------+
-- > |BYWEEKNO  |N/A     |N/A     |N/A    |N/A    |N/A   |N/A    |Expand|
-- > +----------+--------+--------+-------+-------+------+-------+------+
-- > |BYYEARDAY |Limit   |Limit   |Limit  |N/A    |N/A   |N/A    |Expand|
-- > +----------+--------+--------+-------+-------+------+-------+------+
-- > |BYMONTHDAY|Limit   |Limit   |Limit  |Limit  |N/A   |Expand |Expand|
-- > +----------+--------+--------+-------+-------+------+-------+------+
-- > |BYDAY     |Limit   |Limit   |Limit  |Limit  |Expand|Note 1 |Note 2|
-- > +----------+--------+--------+-------+-------+------+-------+------+
-- > |BYHOUR    |Limit   |Limit   |Limit  |Expand |Expand|Expand |Expand|
-- > +----------+--------+--------+-------+-------+------+-------+------+
-- > |BYMINUTE  |Limit   |Limit   |Expand |Expand |Expand|Expand |Expand|
-- > +----------+--------+--------+-------+-------+------+-------+------+
-- > |BYSECOND  |Limit   |Expand  |Expand |Expand |Expand|Expand |Expand|
-- > +----------+--------+--------+-------+-------+------+-------+------+
-- > |BYSETPOS  |Limit   |Limit   |Limit  |Limit  |Limit |Limit  |Limit |
-- > +----------+--------+--------+-------+-------+------+-------+------+
--
-- Note 1:  Limit if BYMONTHDAY is present; otherwise, special expand
--          for MONTHLY.
--
-- Note 2:  Limit if BYYEARDAY or BYMONTHDAY is present; otherwise,
--          special expand for WEEKLY if BYWEEKNO present; otherwise,
--          special expand for MONTHLY if BYMONTH present; otherwise,
--          special expand for YEARLY.
--
--
--
-- Here is an example of evaluating multiple BYxxx rule parts.
--
-- > DTSTART;TZID=America/New_York:19970105T083000
-- > RRULE:FREQ=YEARLY;INTERVAL=2;BYMONTH=1;BYDAY=SU;BYHOUR=8,9;
-- >  BYMINUTE=30
--
-- First, the "INTERVAL=2" would be applied to "FREQ=YEARLY" to
-- arrive at "every other year".  Then, "BYMONTH=1" would be applied
-- to arrive at "every January, every other year".  Then, "BYDAY=SU"
-- would be applied to arrive at "every Sunday in January, every
-- other year".  Then, "BYHOUR=8,9" would be applied to arrive at
-- "every Sunday in January at 8 AM and 9 AM, every other year".
-- Then, "BYMINUTE=30" would be applied to arrive at "every Sunday in
-- January at 8:30 AM and 9:30 AM, every other year".  Then, lacking
-- information from "RRULE", the second is derived from "DTSTART", to
-- end up in "every Sunday in January at 8:30:00 AM and 9:30:00 AM,
-- every other year".  Similarly, if the BYMINUTE, BYHOUR, BYDAY,
-- BYMONTHDAY, or BYMONTH rule part were missing, the appropriate
-- minute, hour, day, or month would have been retrieved from the
-- "DTSTART" property.
--
-- If the computed local start time of a recurrence instance does not
-- exist, or occurs more than once, for the specified time zone, the
-- time of the recurrence instance is interpreted in the same manner
-- as an explicit DATE-TIME value describing that date and time, as
-- specified in Section 3.3.5.
--
-- No additional content value encoding (i.e., BACKSLASH character
-- encoding, see Section 3.3.11) is defined for this value type.
--
-- Example:  The following is a rule that specifies 10 occurrences that
-- occur every other day:
--
-- > FREQ=DAILY;COUNT=10;INTERVAL=2
--
-- There are other examples specified in Section 3.8.5.3.
--
-- = From section 3.8.5.3
--
-- Purpose:  This property defines a rule or repeating pattern for recurring
-- events, to-dos, journal entries, or time zone definitions.
--
-- Conformance:  This property can be specified in recurring "VEVENT", "VTODO",
-- and "VJOURNAL" calendar components as well as in the "STANDARD" and
-- "DAYLIGHT" sub-components of the "VTIMEZONE" calendar component, but it
-- SHOULD NOT be specified more than once.  The recurrence set generated with
-- multiple "RRULE" properties is undefined.
--
-- Description:  The recurrence rule, if specified, is used in computing the
-- recurrence set.  The recurrence set is the complete set of recurrence
-- instances for a calendar component.  The recurrence set is generated by
-- considering the initial "DTSTART" property along with the "RRULE", "RDATE",
-- and "EXDATE" properties contained within the recurring component.  The
-- "DTSTART" property defines the first instance in the recurrence set.  The
-- "DTSTART" property value SHOULD be synchronized with the recurrence rule, if
-- specified.  The recurrence set generated with a "DTSTART" property value not
-- synchronized with the recurrence rule is undefined.  The final recurrence
-- set is generated by gathering all of the start DATE-TIME values generated by
-- any of the specified "RRULE" and "RDATE" properties, and then excluding any
-- start DATE-TIME values specified by "EXDATE" properties.  This implies that
-- start DATE- TIME values specified by "EXDATE" properties take precedence
-- over those specified by inclusion properties (i.e., "RDATE" and "RRULE").
-- Where duplicate instances are generated by the "RRULE" and "RDATE"
-- properties, only one recurrence is considered. Duplicate instances are
-- ignored.
--
-- The "DTSTART" property specified within the iCalendar object defines the
-- first instance of the recurrence.  In most cases, a "DTSTART" property of
-- DATE-TIME value type used with a recurrence rule, should be specified as a
-- date with local time and time zone reference to make sure all the recurrence
-- instances start at the same local time regardless of time zone changes.
--
-- If the duration of the recurring component is specified with the "DTEND" or
-- "DUE" property, then the same exact duration will apply to all the members
-- of the generated recurrence set.  Else, if the duration of the recurring
-- component is specified with the "DURATION" property, then the same nominal
-- duration will apply to all the members of the generated recurrence set and
-- the exact duration of each recurrence instance will depend on its specific
-- start time.  For example, recurrence instances of a nominal duration of one
-- day will have an exact duration of more or less than 24 hours on a day where
-- a time zone shift occurs.  The duration of a specific recurrence may be
-- modified in an exception component or simply by using an "RDATE" property of
-- PERIOD value type.
data RRule
  = RRule
      { -- | The FREQ rule part identifies the type of recurrence rule.
        --
        -- See 'Frequency'
        rRuleFrequency :: !Frequency,
        -- | The INTERVAL rule part contains a positive integer representing at which intervals the recurrence rule repeats.
        --
        -- See 'Interval'
        rRuleInterval :: !Interval,
        -- | This is one haskell-field based on two fields in the spec: UNTIL and COUNT together.
        --
        -- This because the spec says
        --
        -- > "The UNTIL or COUNT rule parts are OPTIONAL, but they MUST NOT occur in the same 'recur'."
        --
        -- See 'UntilCount'
        rRuleUntilCount :: !UntilCount,
        -- | The BYSECOND rule part specifies a COMMA-separated list of seconds within a minute.
        --
        -- See 'BySecond'
        rRuleBySecond :: !(Set BySecond),
        -- | The BYMINUTE rule part specifies a COMMA-separated list of minutes within an hour.
        --
        -- See 'ByMinute'
        rRuleByMinute :: !(Set ByMinute),
        -- | The BYHOUR rule part specifies a COMMA-separated list of hours of the day.
        --
        -- The BYSECOND, BYMINUTE and BYHOUR rule parts MUST NOT be specified when the
        -- associated "DTSTART" property has a DATE value type.  These rule parts MUST
        -- be ignored in RECUR value that violate the above requirement (e.g.,
        -- generated by applications that pre-date this revision of iCalendar).
        --
        -- See 'ByHour'
        rRuleByHour :: !(Set ByHour),
        -- | The BYDAY rule part specifies a COMMA-separated list of days of the week; [...]
        --
        -- The BYDAY rule part MUST NOT be specified with a numeric value when
        -- the FREQ rule part is not set to MONTHLY or YEARLY.  Furthermore,
        -- the BYDAY rule part MUST NOT be specified with a numeric value with
        -- the FREQ rule part set to YEARLY when the BYWEEKNO rule part is
        -- specified.
        --
        -- See 'ByDay'
        rRuleByDay :: !(Set ByDay),
        -- | The BYMONTHDAY rule part specifies a COMMA-separated list of days of the month.
        --
        -- The BYMONTHDAY rule part
        -- MUST NOT be specified when the FREQ rule part is set to WEEKLY
        --
        -- See 'ByMonthDay'
        rRuleByMonthDay :: !(Set ByMonthDay),
        -- | The BYYEARDAY rule part specifies a COMMA-separated list of days of the year.
        --
        -- The BYYEARDAY rule
        -- part MUST NOT be specified when the FREQ rule part is set to DAILY,
        -- WEEKLY, or MONTHLY.
        --
        -- See 'ByYearDay'
        rRuleByYearDay :: !(Set ByYearDay),
        -- | The BYWEEKNO rule part specifies a COMMA-separated list of ordinals specifying weeks of the year.
        --
        -- This rule part MUST NOT be used when
        -- the FREQ rule part is set to anything other than YEARLY.
        --
        -- See 'ByWeekNo'
        rRuleByWeekNo :: !(Set ByWeekNo),
        -- | The BYMONTH rule part specifies a COMMA-separated list of months of the year.
        --
        -- See 'ByMonth'
        rRuleByMonth :: !(Set ByMonth),
        -- | The WKST rule part specifies the day on which the workweek starts.
        --
        -- Valid values are MO, TU, WE, TH, FR, SA, and SU.  This is
        -- significant when a WEEKLY "RRULE" has an interval greater than 1,
        -- and a BYDAY rule part is specified.  This is also significant when
        -- in a YEARLY "RRULE" when a BYWEEKNO rule part is specified.  The
        -- default value is MO.
        --
        -- Note: We did not chose 'Maybe DayOfWeek' because that would have two ways to represent the default value.
        rRuleWeekStart :: !DayOfWeek,
        -- | The BYSETPOS rule part specifies a COMMA-separated list of values
        -- that corresponds to the nth occurrence within the set of recurrence
        -- instances specified by the rule.
        --
        -- It MUST only be used in conjunction with another BYxxx rule part.
        --
        -- See 'BySetPos'
        rRuleBySetPos :: !(Set BySetPos)
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity RRule where
  validate rule@RRule {..} =
    mconcat
      [ genericValidate rule,
        decorateList (S.toList rRuleByDay) $ \bd ->
          declare "The BYDAY rule part MUST NOT be specified with a numeric value when the FREQ rule part is not set to MONTHLY or YEARLY." $
            let care = case bd of
                  Every _ -> True
                  Specific _ _ -> False
             in case rRuleFrequency of
                  Monthly -> care
                  Yearly -> care
                  _ -> True,
        declare "The BYMONTHDAY rule part MUST NOT be specified when the FREQ rule part is set to WEEKLY" $
          case rRuleFrequency of
            Weekly -> S.null rRuleByMonthDay
            _ -> True,
        declare "The BYYEARDAY rule part MUST NOT be specified when the FREQ rule part is set to DAILY, WEEKLY, or MONTHLY." $
          case rRuleFrequency of
            Daily -> S.null rRuleByYearDay
            Weekly -> S.null rRuleByYearDay
            Monthly -> S.null rRuleByYearDay
            _ -> True,
        declare "The BYWEEKNO rule part MUST NOT be used when the FREQ rule part is set to anything other than YEARLY." $
          case rRuleFrequency of
            Yearly -> True
            _ -> null rRuleByWeekNo,
        declare "the BYSETPOST rule part MUST only be used in conjunction with another BYxxx rule part." $
          if S.null rRuleBySetPos
            then True
            else
              any
                not
                [ S.null rRuleBySecond,
                  S.null rRuleByMinute,
                  S.null rRuleByHour,
                  S.null rRuleByDay,
                  S.null rRuleByMonthDay,
                  S.null rRuleByYearDay,
                  S.null rRuleByWeekNo,
                  S.null rRuleByMonth
                ]
      ]

instance YamlSchema RRule where
  yamlSchema =
    objectParser "RRule" $
      RRule
        <$> requiredField' "frequency"
        <*> optionalFieldWithDefault' "interval" (Interval 1)
        <*> untilCountObjectParser
        <*> optionalFieldWithDefault' "second" S.empty
        <*> optionalFieldWithDefault' "minute" S.empty
        <*> optionalFieldWithDefault' "hour" S.empty
        <*> optionalFieldWithDefault' "day" S.empty
        <*> optionalFieldWithDefault' "monthday" S.empty
        <*> optionalFieldWithDefault' "yearday" S.empty
        <*> optionalFieldWithDefault' "weekno" S.empty
        <*> optionalFieldWithDefault' "month" S.empty
        <*> optionalFieldWithDefault' "week-start" Monday
        <*> optionalFieldWithDefault' "setpos" S.empty

instance FromJSON RRule where
  parseJSON = viaYamlSchema

instance ToJSON RRule where
  toJSON RRule {..} =
    object $
      concat
        [ [ "frequency" .= rRuleFrequency,
            "interval" .= rRuleInterval
          ],
          setPair "second" rRuleBySecond,
          setPair "minute" rRuleByMinute,
          setPair "hour" rRuleByHour,
          setPair "day" rRuleByDay,
          setPair "monthday" rRuleByMonthDay,
          setPair "yearday" rRuleByYearDay,
          setPair "weekno" rRuleByWeekNo,
          setPair "month" rRuleByMonth,
          ["week-start" .= rRuleWeekStart],
          setPair "setpos" rRuleBySetPos
        ]
        ++ untilCountObject rRuleUntilCount
    where
      setPair k s = [k .= s | not (S.null s)]

-- | Frequency
--
-- This rule part MUST be specified in the recurrence rule.  Valid
-- values include SECONDLY, to specify repeating events based on an
-- interval of a second or more; MINUTELY, to specify repeating events
-- based on an interval of a minute or more; HOURLY, to specify
-- repeating events based on an interval of an hour or more; DAILY, to
-- specify repeating events based on an interval of a day or more;
-- WEEKLY, to specify repeating events based on an interval of a week
-- or more; MONTHLY, to specify repeating events based on an interval
-- of a month or more; and YEARLY, to specify repeating events based on
-- an interval of a year or more.
data Frequency
  = Secondly
  | Minutely
  | Hourly
  | Daily
  | Weekly
  | Monthly
  | Yearly
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

instance Validity Frequency

instance YamlSchema Frequency where
  yamlSchema =
    alternatives
      [ literalValue Secondly,
        literalValue Minutely,
        literalValue Hourly,
        literalValue Daily,
        literalValue Weekly,
        literalValue Monthly,
        literalValue Yearly
      ]

instance FromJSON Frequency where
  parseJSON = viaYamlSchema

instance ToJSON Frequency

-- | Interval
--
-- The default value is "1", meaning every second for a SECONDLY rule,
-- every minute for a MINUTELY rule, every hour for an HOURLY rule,
-- every day for a DAILY rule, every week for a WEEKLY rule, every
-- month for a MONTHLY rule, and every year for a YEARLY rule.  For
-- example, within a DAILY rule, a value of "8" means every eight days.
--
-- Note: We did not chose 'Maybe Word' because that would have two ways to represent the default value.
newtype Interval = Interval {unInterval :: Word}
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance Validity Interval where
  validate i@(Interval w) = mconcat [genericValidate i, declare "The interval is not zero" $ w /= 0]

instance YamlSchema Interval where
  yamlSchema = Interval <$> yamlSchema

data UntilCount
  = -- | The UNTIL rule part defines a DATE or DATE-TIME value that bounds the recurrence rule in an inclusive manner.
    --
    -- If the value specified by UNTIL is synchronized with the specified
    -- recurrence, this DATE or DATE-TIME becomes the last instance of the
    -- recurrence.  The value of the UNTIL rule part MUST have the same value
    -- type as the "DTSTART" property.  Furthermore, if the "DTSTART" property
    -- is specified as a date with local time, then the UNTIL rule part MUST
    -- also be specified as a date with local time.  If the "DTSTART" property
    -- is specified as a date with UTC time or a date with local time and time
    -- zone reference, then the UNTIL rule part MUST be specified as a date
    -- with UTC time.  In the case of the "STANDARD" and "DAYLIGHT"
    -- sub-components the UNTIL rule part MUST always be specified as a date
    -- with UTC time.  If specified as a DATE-TIME value, then it MUST be
    -- specified in a UTC time format.
    Until LocalTime
  | -- | The COUNT rule part defines the number of occurrences at which to range-bound the recurrence.
    --
    -- The "DTSTART" property value always counts as the first occurrence.
    Count Word
  | -- | If [the UNTIL rule part is] not present, and the COUNT rule part is also not present, the "RRULE" is considered to repeat forever.
    Indefinitely
  deriving (Show, Eq, Ord, Generic)

instance Validity UntilCount

instance YamlSchema UntilCount where
  yamlSchema = objectParser "UntilCount" untilCountObjectParser

untilCountObjectParser :: ObjectParser UntilCount
untilCountObjectParser =
  alternatives
    [ Until <$> requiredFieldWith' "until" localTimeSchema,
      Count <$> requiredField' "count",
      pure Indefinitely
    ]

instance FromJSON UntilCount where
  parseJSON = viaYamlSchema

instance ToJSON UntilCount where
  toJSON = object . untilCountObject

untilCountObject :: UntilCount -> [Pair]
untilCountObject = \case
  Until lt -> ["until" .= formatTime defaultTimeLocale timestampLocalTimeFormat lt]
  Count c -> ["count" .= c]
  Indefinitely -> []

-- | A second within a minute
--
-- Valid values are 0 to 60.
newtype BySecond = Second {unSecond :: Word}
  deriving (Show, Eq, Ord, Generic, ToJSON)

instance Validity BySecond where
  validate s@(Second w) =
    mconcat
      [ genericValidate s,
        declare "Valid values are 0 to 60." $ w >= 0 && w <= 60
      ]

instance YamlSchema BySecond where
  yamlSchema = Second <$> yamlSchema

instance FromJSON BySecond where
  parseJSON = viaYamlSchema

-- | A minute within an hour
--
-- Valid values are 0 to 59.
newtype ByMinute = Minute {unMinute :: Word}
  deriving (Show, Eq, Ord, Generic, ToJSON)

instance Validity ByMinute where
  validate s@(Minute w) =
    mconcat
      [ genericValidate s,
        declare "Valid values are 0 to 59." $ w >= 0 && w <= 59
      ]

instance YamlSchema ByMinute where
  yamlSchema = Minute <$> yamlSchema

instance FromJSON ByMinute where
  parseJSON = viaYamlSchema

-- | An hour within a day
--
-- Valid values are 0 to 23.
newtype ByHour = Hour {unHour :: Word}
  deriving (Show, Eq, Ord, Generic, ToJSON)

instance Validity ByHour where
  validate s@(Hour w) =
    mconcat
      [ genericValidate s,
        declare "Valid values are 0 to 23." $ w >= 0 && w <= 23
      ]

instance YamlSchema ByHour where
  yamlSchema = Hour <$> yamlSchema

instance FromJSON ByHour where
  parseJSON = viaYamlSchema

-- | The BYDAY rule part specifies a COMMA-separated list of days of the week;
--
-- Each BYDAY value can also be preceded by a positive (+n) or
-- negative (-n) integer.  If present, this indicates the nth
-- occurrence of a specific day within the MONTHLY or YEARLY "RRULE".
--
-- For example, within a MONTHLY rule, +1MO (or simply 1MO)
-- represents the first Monday within the month, whereas -1MO
-- represents the last Monday of the month.  The numeric value in a
-- BYDAY rule part with the FREQ rule part set to YEARLY corresponds
-- to an offset within the month when the BYMONTH rule part is
-- present, and corresponds to an offset within the year when the
-- BYWEEKNO or BYMONTH rule parts are present.  If an integer
-- modifier is not present, it means all days of this type within the
-- specified frequency.  For example, within a MONTHLY rule, MO
-- represents all Mondays within the month.
data ByDay
  = Every DayOfWeek
  | Specific Int DayOfWeek
  deriving (Show, Eq, Ord, Generic)

instance Validity ByDay where
  validate bd =
    mconcat
      [ genericValidate bd,
        case bd of
          Every _ -> valid
          Specific i _ -> declare "The specific weekday number is not zero" $ i /= 0
      ]

instance YamlSchema ByDay where
  yamlSchema =
    objectParser "ByDay" $
      alternatives
        [ Specific <$> requiredField' "pos" <*> requiredField' "day",
          Every <$> requiredField' "day"
        ]

instance FromJSON ByDay where
  parseJSON = viaYamlSchema

instance ToJSON ByDay where
  toJSON = \case
    Every d -> object ["day" .= d]
    Specific p d -> object ["pos" .= p, "day" .= d]

-- | A day within a month
--
-- Valid values are 1 to 31 or -31 to -1.  For example, -10 represents the
-- tenth to the last day of the month.
newtype ByMonthDay = MonthDay {unMonthDay :: Int}
  deriving (Show, Eq, Ord, Generic, ToJSON)

instance Validity ByMonthDay where
  validate md@(MonthDay i) =
    mconcat
      [ genericValidate md,
        declare "Valid values are 1 to 31 or -31 to -1." $ i /= 0 && i >= -31 && i <= 31
      ]

instance YamlSchema ByMonthDay where
  yamlSchema = MonthDay <$> yamlSchema

instance FromJSON ByMonthDay where
  parseJSON = viaYamlSchema

-- | A day within a year
--
-- Valid values are 1 to 366 or -366 to -1.  For example, -1 represents the
-- last day of the year (December 31st) and -306 represents the 306th to the
-- last day of the year (March 1st).
newtype ByYearDay = YearDay {unYearDay :: Int}
  deriving (Show, Eq, Ord, Generic, ToJSON)

instance Validity ByYearDay where
  validate md@(YearDay i) =
    mconcat
      [ genericValidate md,
        declare "Valid values are 1 to 366 or -366 to -1." $ i /= 0 && i >= -366 && i <= 366
      ]

instance YamlSchema ByYearDay where
  yamlSchema = YearDay <$> yamlSchema

instance FromJSON ByYearDay where
  parseJSON = viaYamlSchema

-- | A week within a year
--
-- Valid values are 1 to 53 or -53 to -1.  This corresponds to weeks according
-- to week numbering as defined in
-- [ISO.8601.2004](https://tools.ietf.org/html/rfc5545#ref-ISO.8601.2004).  A
-- week is defined as a seven day period, starting on the day of the week
-- defined to be the week start (see WKST).  Week number one of the calendar
-- year is the first week that contains at least four (4) days in that calendar
-- year.
--
-- For example, 3 represents the third week of the year.
--
-- Note: Assuming a Monday week start, week 53 can only occur when Thursday is
-- January 1 or if it is a leap year and Wednesday is January 1.
newtype ByWeekNo = WeekNo {unWeekNo :: Int}
  deriving (Show, Eq, Ord, Generic, ToJSON)

instance Validity ByWeekNo where
  validate bwn@(WeekNo i) =
    mconcat
      [ genericValidate bwn,
        declare "Valid values are 1 to 53 or -53 to -1." $ i /= 0 && i >= -53 && i <= 53
      ]

instance YamlSchema ByWeekNo where
  yamlSchema = WeekNo <$> yamlSchema

instance FromJSON ByWeekNo where
  parseJSON = viaYamlSchema

-- | A month within a year
--
-- Valid values are 1 to 12.
--
-- In Haskell we represent these using a 'Month' value.
type ByMonth = Month

-- | A position within the recurrence set
--
-- BYSETPOS operates on
-- a set of recurrence instances in one interval of the recurrence
-- rule.  For example, in a WEEKLY rule, the interval would be one
-- week A set of recurrence instances starts at the beginning of the
-- interval defined by the FREQ rule part.  Valid values are 1 to 366
-- or -366 to -1.
--
-- For example "the last work day of the month"
-- could be represented as:
--
--  FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1
--
-- Each BYSETPOS value can include a positive (+n) or negative (-n)
-- integer.  If present, this indicates the nth occurrence of the
-- specific occurrence within the set of occurrences specified by the
-- rule.
newtype BySetPos = SetPos {unSetPos :: Int}
  deriving (Show, Eq, Ord, Generic, ToJSON)

instance Validity BySetPos where
  validate sp@(SetPos w) =
    mconcat
      [ genericValidate sp,
        declare "The set position is not zero" $ w /= 0
      ]

instance YamlSchema BySetPos where
  yamlSchema = SetPos <$> yamlSchema

instance FromJSON BySetPos where
  parseJSON = viaYamlSchema

-- A month within a year
--
-- Until 'time' has this too'
data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Validity Month where
  validate = trivialValidation

instance YamlSchema Month where
  yamlSchema =
    alternatives
      [ literalValue January,
        literalValue February,
        literalValue March,
        literalValue April,
        literalValue May,
        literalValue June,
        literalValue July,
        literalValue August,
        literalValue September,
        literalValue October,
        literalValue November,
        literalValue December
      ]

instance FromJSON Month where
  parseJSON = viaYamlSchema

instance ToJSON Month

deriving instance Ord DayOfWeek -- Silly that this doesn't exist. We need to be able to put days in a set

deriving instance Generic DayOfWeek

instance Validity DayOfWeek where -- Until we have it in validity-time
  validate = trivialValidation

instance YamlSchema DayOfWeek where -- Until we have it in yamlparse-applicative
  yamlSchema =
    alternatives
      [ literalValue Monday,
        literalValue Tuesday,
        literalValue Wednesday,
        literalValue Thursday,
        literalValue Friday,
        literalValue Saturday,
        literalValue Sunday
      ]

monthToMontNo :: Month -> Int
monthToMontNo = \case
  January -> 1
  February -> 2
  March -> 3
  April -> 4
  May -> 5
  June -> 6
  July -> 7
  August -> 8
  September -> 9
  October -> 10
  November -> 11
  December -> 12

monthNoToMonth :: Int -> Maybe Month
monthNoToMonth = \case
  1 -> Just January
  2 -> Just February
  3 -> Just March
  4 -> Just April
  5 -> Just May
  6 -> Just June
  7 -> Just July
  8 -> Just August
  9 -> Just September
  10 -> Just October
  11 -> Just November
  12 -> Just December
  _ -> Nothing

rRule :: Frequency -> RRule
rRule freq =
  RRule
    { rRuleFrequency = freq,
      rRuleInterval = Interval 1,
      rRuleUntilCount = Indefinitely,
      rRuleBySecond = S.empty,
      rRuleByMinute = S.empty,
      rRuleByHour = S.empty,
      rRuleByDay = S.empty,
      rRuleByMonthDay = S.empty,
      rRuleByYearDay = S.empty,
      rRuleByWeekNo = S.empty,
      rRuleByMonth = S.empty,
      rRuleWeekStart = Monday,
      rRuleBySetPos = S.empty
    }

-- Recurrence rules operate on LocalTime instead of CalDateTime because of this line in the spec:
--
-- The "DTSTART" property specified within the iCalendar object defines the
-- first instance of the recurrence.  In most cases, a "DTSTART" property of
-- DATE-TIME value type used with a recurrence rule, should be specified as a
-- date with local time and time zone reference to make sure all the recurrence
-- instances start at the same local time regardless of time zone changes.
--
-- This function takes care of the 'rRuleUntilCount' part.
rruleDateTimeOccurrencesUntil ::
  -- | DTStart
  LocalTime ->
  -- | recurrence rule
  RRule ->
  -- | Limit
  LocalTime ->
  -- | The recurrence set.
  -- For infinte recurrence sets, these are only the occurrences before (inclusive) the limit.
  Set LocalTime
rruleDateTimeOccurrencesUntil = occurrencesUntil rruleDateTimeNextOccurrence (<=)

rruleDateOccurrencesUntil ::
  -- | DTStart
  Day ->
  -- | recurrence rule
  RRule ->
  -- | Limit
  Day ->
  -- | The recurrence set.
  -- For infinte recurrence sets, these are only the occurrences before (inclusive) the limit.
  Set Day
rruleDateOccurrencesUntil = occurrencesUntil rruleDateNextOccurrence (\d lt -> d <= localDay lt)

occurrencesUntil :: Ord a => (a -> a -> RRule -> Maybe a) -> (a -> LocalTime -> Bool) -> a -> RRule -> a -> Set a
occurrencesUntil func leFunc start rrule limit = case rRuleUntilCount rrule of
  Indefinitely -> goIndefinitely
  Count i -> goCount i
  Until lt -> goUntil lt
  where
    goUntil untilLimit = S.filter (`leFunc` untilLimit) goIndefinitely
    goCount count = S.take (fromIntegral count) goIndefinitely
    goIndefinitely = iterateMaybeSet (\cur -> func cur limit rrule) start

iterateMaybeSet :: Ord a => (a -> Maybe a) -> a -> Set a
iterateMaybeSet func start = go start
  where
    go cur = case func cur of
      Nothing -> S.singleton start
      Just next -> S.insert next $ go next

-- This function takes care of the 'rRuleFrequency' part.
rruleDateTimeNextOccurrence :: LocalTime -> LocalTime -> RRule -> Maybe LocalTime
rruleDateTimeNextOccurrence lt limit RRule {..} = case rRuleFrequency of
  -- 1. From the spec:
  --
  --    > The BYDAY rule part MUST NOT be specified with a numeric value when
  --    > the FREQ rule part is not set to MONTHLY or YEARLY.  Furthermore,
  --
  --    So we 'filterEvery' on the 'byDay's for every frequency except 'MONTHLY' and 'YEARLY'.
  Daily -> dailyDateTimeNextRecurrence lt limit rRuleInterval rRuleByMonth rRuleByMonthDay (filterEvery rRuleByDay) rRuleByHour rRuleByMinute rRuleBySecond rRuleBySetPos
  Weekly -> weeklyDateTimeNextRecurrence lt limit rRuleInterval rRuleByMonth rRuleWeekStart (filterEvery rRuleByDay) rRuleByHour rRuleByMinute rRuleBySecond rRuleBySetPos
  Monthly -> monthlyDateTimeNextRecurrence lt limit rRuleInterval rRuleByMonth rRuleByMonthDay rRuleByDay rRuleByHour rRuleByMinute rRuleBySecond rRuleBySetPos
  _ -> Nothing

rruleDateNextOccurrence :: Day -> Day -> RRule -> Maybe Day
rruleDateNextOccurrence d limit RRule {..} =
  case rRuleFrequency of
    -- 1. From the spec:
    --
    --    > The BYDAY rule part MUST NOT be specified with a numeric value when
    --    > the FREQ rule part is not set to MONTHLY or YEARLY.  Furthermore,
    --
    --    So we 'filterEvery' on the 'byDay's for every frequency except 'MONTHLY' and 'YEARLY'.
    --
    -- 2. By set pos is ignored because every day is the only day in a daily interval
    Daily -> dailyDateNextRecurrence d limit rRuleInterval rRuleByMonth rRuleByMonthDay (filterEvery rRuleByDay) -- By set pos is ignored because every day is the only day in a daily interval
    Weekly -> weeklyDateNextRecurrence d limit rRuleInterval rRuleByMonth rRuleWeekStart (filterEvery rRuleByDay) rRuleBySetPos
    Monthly -> monthlyDateNextRecurrence d limit rRuleInterval rRuleByMonth rRuleByMonthDay rRuleByDay rRuleBySetPos
    _ -> Nothing

filterEvery :: Set ByDay -> Set DayOfWeek
filterEvery =
  S.fromList
    . mapMaybe
      ( \case
          Every d -> Just d
          _ -> Nothing
      )
    . S.toList

-- | Recur with a 'Daily' frequency
dailyDateTimeNextRecurrence ::
  LocalTime ->
  LocalTime ->
  Interval ->
  Set ByMonth ->
  Set ByMonthDay ->
  Set DayOfWeek ->
  Set ByHour ->
  Set ByMinute ->
  Set BySecond ->
  Set BySetPos ->
  Maybe LocalTime
dailyDateTimeNextRecurrence
  lt@(LocalTime d_ tod_)
  limit@(LocalTime limitDay _)
  interval
  byMonths
  byMonthDays
  byDays
  byHours
  byMinutes
  bySeconds
  bySetPoss = headMay $ do
    d <- dailyDayRecurrence d_ limitDay interval byMonths byMonthDays byDays
    tod <- filterSetPos bySetPoss $ timeOfDayExpand tod_ byHours byMinutes bySeconds
    let next = LocalTime d tod
    guard (next > lt) -- Don't take the current one again
    guard (next <= limit) -- Don't go beyond the limit
    pure next

-- | Recur with a 'Daily' frequency
dailyDateNextRecurrence ::
  Day ->
  Day ->
  Interval ->
  Set ByMonth ->
  Set ByMonthDay ->
  Set DayOfWeek ->
  Maybe Day
dailyDateNextRecurrence
  d_
  limitDay
  interval
  byMonths
  byMonthDays
  byDays =
    headMay $ do
      d <- dailyDayRecurrence d_ limitDay interval byMonths byMonthDays byDays
      guard (d > d_) -- Don't take the current one again
      guard (d <= limitDay) -- Don't go beyond the limit
      pure d

-- | Internal: Get all the relevant days until the limit, not considering any 'Set BySetPos'
dailyDayRecurrence ::
  Day ->
  Day ->
  Interval ->
  Set Month ->
  Set ByMonthDay ->
  Set DayOfWeek ->
  [Day]
dailyDayRecurrence
  d_
  limitDay
  (Interval interval)
  byMonths
  byMonthDays
  byDays = do
    d <- takeWhile (<= limitDay) $ map (\i -> addDays (fromIntegral interval * i) d_) [0 ..]
    guard $ byMonthLimit byMonths d
    guard $ byMonthDayLimit byMonthDays d
    guard $ byEveryWeekDayLimit byDays d
    pure d

-- | Recur with a 'Weekly' frequency
weeklyDateTimeNextRecurrence ::
  LocalTime ->
  LocalTime ->
  Interval ->
  Set ByMonth ->
  DayOfWeek ->
  Set DayOfWeek ->
  Set ByHour ->
  Set ByMinute ->
  Set BySecond ->
  Set BySetPos ->
  Maybe LocalTime
weeklyDateTimeNextRecurrence
  lt@(LocalTime d_ tod_)
  limit@(LocalTime limitDay _)
  interval
  byMonths
  weekStart
  byDays
  byHours
  byMinutes
  bySeconds
  bySetPoss =
    headMay $ do
      d <- weeklyDayRecurrence d_ limitDay interval byMonths weekStart byDays bySetPoss
      tod <- timeOfDayExpand tod_ byHours byMinutes bySeconds
      let next = LocalTime d tod
      guard (next > lt) -- Don't take the current one again
      guard (next <= limit) -- Don't go beyond the limit
      pure next

-- | Recur with a 'Weekly' frequency
weeklyDateNextRecurrence ::
  Day ->
  Day ->
  Interval ->
  Set ByMonth ->
  DayOfWeek ->
  Set DayOfWeek ->
  Set BySetPos ->
  Maybe Day
weeklyDateNextRecurrence
  d_
  limitDay
  interval
  byMonths
  weekStart
  byDays
  bySetPoss =
    headMay $ do
      d <- weeklyDayRecurrence d_ limitDay interval byMonths weekStart byDays bySetPoss
      guard (d > d_) -- Don't take the current one again
      pure d

-- | Internal: Get all the relevant days until the limit, not considering any 'Set BySetPos'
weeklyDayRecurrence ::
  Day ->
  Day ->
  Interval ->
  Set Month ->
  DayOfWeek ->
  Set DayOfWeek ->
  Set BySetPos ->
  [Day]
weeklyDayRecurrence
  d_
  limitDay
  (Interval interval)
  byMonths
  weekStart
  byDays
  bySetPoss = do
    let (y, w, dow) = toWeekDateWithStart weekStart d_
    let (limitY, limitWN, _) = toWeekDateWithStart weekStart limitDay
    (y', w') <-
      takeEvery interval
        $ takeWhile (<= (limitY, limitWN))
        $ weeksIntoTheFutureStartingFrom weekStart y w
    d <-
      filterSetPos bySetPoss
        $ sort -- Need to sort because the week days may not be in order.
        $ if S.null byDays
          then maybeToList $ fromWeekDateWithStart weekStart y' w' dow
          else do
            dow' <- S.toList byDays
            maybeToList $ fromWeekDateWithStart weekStart y' w' dow'
    guard $ byMonthLimit byMonths d
    guard (d <= limitDay) -- Don't go beyond the limit
    pure d

monthlyDateTimeNextRecurrence ::
  LocalTime ->
  LocalTime ->
  Interval ->
  Set ByMonth ->
  Set ByMonthDay ->
  Set ByDay ->
  Set ByHour ->
  Set ByMinute ->
  Set BySecond ->
  Set BySetPos ->
  Maybe LocalTime
monthlyDateTimeNextRecurrence
  lt@(LocalTime d_ tod_)
  limit@(LocalTime limitDay _)
  interval
  byMonths
  byMonthDays
  byDays
  byHours
  byMinutes
  bySeconds
  bySetPoss =
    headMay $ do
      d <- monthlyDayRecurrence d_ limitDay interval byMonths byMonthDays byDays bySetPoss
      tod <- timeOfDayExpand tod_ byHours byMinutes bySeconds
      let next = LocalTime d tod
      guard (next > lt) -- Don't take the current one again
      guard (next <= limit) -- Don't go beyond the limit
      pure next

monthlyDateNextRecurrence ::
  Day ->
  Day ->
  Interval ->
  Set ByMonth ->
  Set ByMonthDay ->
  Set ByDay ->
  Set BySetPos ->
  Maybe Day
monthlyDateNextRecurrence
  d_
  limitDay
  interval
  byMonths
  byMonthDays
  byDays
  bySetPoss =
    headMay $ do
      d <- monthlyDayRecurrence d_ limitDay interval byMonths byMonthDays byDays bySetPoss
      guard (d > d_) -- Don't take the current one again
      pure d

monthlyDayRecurrence ::
  Day ->
  Day ->
  Interval ->
  Set ByMonth ->
  Set ByMonthDay ->
  Set ByDay ->
  Set BySetPos ->
  [Day]
monthlyDayRecurrence
  d_
  limitDay
  (Interval interval)
  byMonths
  byMonthDays
  byDays
  bySetPoss = do
    let (year_, month_, md_) = toGregorian d_
    let (limitYear, limitMonth, _) = toGregorian limitDay
    (year, month) <- filterSetPos bySetPoss $ takeEvery interval $ takeWhile (<= (limitYear, limitMonth)) $ dropWhile (< (year_, month_)) $ do
      y <- [year_ .. limitYear]
      m <- [1 .. 12]
      pure (y, m)
    m <- maybeToList $ monthNoToMonth month
    guard $ byMonthLimitMonth byMonths m
    md <- byMonthDayExpand year m md_ byMonthDays
    d <- maybeToList $ fromGregorianValid year month md
    d' <-
      if S.null byMonthDays
        then do
          guard $ byDayLimit byDays d
          pure d
        else byDayExand d byDays
    guard (d' <= limitDay) -- Don't go beyond the limit
    pure d'

takeEvery :: Word -> [a] -> [a]
takeEvery i = go 0
  where
    go _ [] = []
    go 0 (l : ls) = l : go (i -1) ls
    go j (_ : ls) = go (pred j) ls

byMonthLimitMonth :: Set ByMonth -> Month -> Bool
byMonthLimitMonth = limitBy $ \m1 m2 -> m1 == m2

byMonthLimit :: Set ByMonth -> Day -> Bool
byMonthLimit = limitBy $ \d m ->
  let (_, month, _) = toGregorian d
   in month == monthToMontNo m

byMonthDayLimit :: Set ByMonthDay -> Day -> Bool
byMonthDayLimit = limitBy $ \d (MonthDay md) ->
  let (positiveMonthDayIndex, negativeMonthDayIndex) = monthIndices d
   in positiveMonthDayIndex == md
        || negativeMonthDayIndex == md

monthIndices :: Day -> (Int, Int) -- (Positive index, Negative index)
monthIndices d =
  let (y, month, day) = toGregorian d
      leap = isLeapYear y
      monthLen = monthLength leap month
      negativeMonthDayIndex = negate $ monthLen - day + 1
   in (day, negativeMonthDayIndex)

byDayLimit :: Set ByDay -> Day -> Bool
byDayLimit = limitBy $ \d bd -> case bd of
  Every dow -> dayOfWeek d == dow
  Specific i dow ->
    let (pos, neg) = specificWeekDayIndex d dow
     in i == pos || i == neg

byEveryWeekDayLimit :: Set DayOfWeek -> Day -> Bool
byEveryWeekDayLimit = limitBy $ \d dow -> dow == dayOfWeek d

byDayExand :: Day -> Set ByDay -> [Day]
byDayExand = expandM $ const undefined

byMonthDayExpand :: Integer -> Month -> Int -> Set ByMonthDay -> [Int]
byMonthDayExpand y m = expandM $ \(MonthDay md) ->
  let len = monthLength (isLeapYear y) (monthToMontNo m)
   in case compare md 0 of
        EQ -> Nothing -- Should not happen
        LT ->
          -- Negative
          Just $ len - md
        GT -> Just md

timeOfDayExpand :: TimeOfDay -> Set ByHour -> Set ByMinute -> Set BySecond -> [TimeOfDay]
timeOfDayExpand (TimeOfDay h_ m_ s_) byHours byMinutes bySeconds = do
  h <- byHourExpand h_ byHours
  m <- byMinuteExpand m_ byMinutes
  s <- bySecondExpand s_ bySeconds
  let tod = TimeOfDay h m s
  pure tod

byHourExpand :: Int -> Set ByHour -> [Int]
byHourExpand = expand (fromIntegral . unHour)

byMinuteExpand :: Int -> Set ByMinute -> [Int]
byMinuteExpand = expand (fromIntegral . unMinute)

bySecondExpand :: Pico -> Set BySecond -> [Pico]
bySecondExpand = expand (fromIntegral . unSecond)

expand :: (b -> a) -> a -> Set b -> [a]
expand func def bys = if S.null bys then pure def else map func (S.toList bys)

expandM :: (b -> Maybe a) -> a -> Set b -> [a]
expandM func def bys = if S.null bys then pure def else mapMaybe func (S.toList bys)

expandL :: (b -> [a]) -> a -> Set b -> [a]
expandL func def bys = if S.null bys then pure def else concatMap func (S.toList bys)

filterSetPos :: Set BySetPos -> [a] -> [a]
filterSetPos poss values =
  map snd (filter (limitBy func poss) (zip [1 ..] values))
  where
    len = length values
    func (i, _) (SetPos pos) =
      let toNegative positive = negate $ len - positive + 1
       in i == pos || toNegative i == pos

limitBy :: (b -> a -> Bool) -> Set a -> b -> Bool
limitBy func bys b =
  if S.null bys
    then True
    else any (func b) bys

-- This can probably be sped up a lot using the weekdate module
specificWeekDayIndex :: Day -> DayOfWeek -> (Int, Int) -- (Positive index, Negative index)
specificWeekDayIndex d wd =
  let (y, month, _) = toGregorian d
      firstDayOfTheMonth = fromGregorian y month 1
      lastDayOfTheMonth = fromGregorian y month 31 -- Will be clipped
      daysOfThisMonth = numberWeekdays [firstDayOfTheMonth .. lastDayOfTheMonth]
      numberOfThisWeekDayInTheMonth = length $ filter ((== wd) . fst . snd) daysOfThisMonth
      (_, positiveSpecificWeekDayIndex) = fromJust (lookup d daysOfThisMonth) -- Must be there
   in (positiveSpecificWeekDayIndex, numberOfThisWeekDayInTheMonth - positiveSpecificWeekDayIndex)
  where
    numberWeekdays :: [Day] -> [(Day, (DayOfWeek, Int))]
    numberWeekdays = go M.empty
      where
        go _ [] = []
        go m (d_ : ds) =
          let dow = dayOfWeek d_
              (mv, m') =
                M.insertLookupWithKey
                  (\_ _ old -> succ old) -- If found, just increment
                  dow
                  1 -- If not found, insert 1
                  m
           in (d_, (dow, fromMaybe 1 mv)) : go m' ds
