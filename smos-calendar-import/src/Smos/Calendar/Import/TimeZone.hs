{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.TimeZone where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import Data.Validity
import GHC.Generics
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.UnresolvedTimestamp
import Smos.Data

newtype TimeZoneHistory = TimeZoneHistory {timeZoneHistoryRules :: [TimeZoneHistoryRule]}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TimeZoneHistory)

instance Validity TimeZoneHistory

instance HasCodec TimeZoneHistory where
  codec = dimapCodec f g $ eitherCodec codec codec
    where
      f = \case
        Left l -> TimeZoneHistory l
        Right e -> TimeZoneHistory [e]
      g = \case
        TimeZoneHistory [e] -> Right e
        TimeZoneHistory l -> Left l

data TimeZoneHistoryRule = TimeZoneHistoryRule
  { timeZoneHistoryRuleStart :: !LocalTime, -- In the timezone at the time
    timeZoneHistoryRuleOffsetFrom :: !UTCOffset,
    timeZoneHistoryRuleOffsetTo :: !UTCOffset,
    timeZoneHistoryRuleRRules :: !(Set RRule),
    timeZoneHistoryRuleRDates :: !(Set CalRDate)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TimeZoneHistoryRule)

instance Validity TimeZoneHistoryRule

instance HasCodec TimeZoneHistoryRule where
  codec =
    named "TimeZoneHistoryRule" $
      object "TimeZoneHistoryRule" $
        TimeZoneHistoryRule
          <$> requiredFieldWith' "start" localTimeCodec .= timeZoneHistoryRuleStart
          <*> requiredField' "from" .= timeZoneHistoryRuleOffsetFrom
          <*> requiredField' "to" .= timeZoneHistoryRuleOffsetTo
          <*> optionalFieldWithOmittedDefault' "rules" S.empty .= timeZoneHistoryRuleRRules
          <*> optionalFieldWithOmittedDefault' "rdates" S.empty .= timeZoneHistoryRuleRDates

newtype UTCOffset = UTCOffset {unUTCOffset :: Int} -- Minutes from UTCTime
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec UTCOffset)

instance Validity UTCOffset

instance HasCodec UTCOffset where
  codec = dimapCodec UTCOffset unUTCOffset codec
