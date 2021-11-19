{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Clock.Types where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Time
import Data.Validity
import Data.Validity.Path ()
import Data.Yaml.Builder (ToYaml)
import GHC.Generics (Generic)
import Path
import Smos.Data
import Smos.Report.TimeBlock

-- Note: the order of these constructors matters
data TemporalClockResolution
  = TemporalSecondsResolution
  | TemporalMinutesResolution
  | TemporalHoursResolution
  deriving (Show, Eq, Ord, Generic)

instance Validity TemporalClockResolution

instance ToJSON TemporalClockResolution

data DecimalClockResolution
  = DecimalHoursResolution
  | DecimalQuarterResolution
  | DecimalResolution Word -- Number of significant digits
  deriving (Show, Eq, Ord, Generic)

instance Validity DecimalClockResolution

instance ToJSON DecimalClockResolution

data ClockFormat
  = ClockFormatTemporal TemporalClockResolution
  | ClockFormatDecimal DecimalClockResolution
  deriving (Show, Eq, Ord, Generic)

instance Validity ClockFormat

instance ToJSON ClockFormat

data ClockReportStyle
  = ClockForest
  | ClockFlat
  deriving (Show, Eq, Ord, Generic)

instance Validity ClockReportStyle

instance ToJSON ClockReportStyle

type ClockTable = [ClockTableBlock]

type ClockTableBlock = Block Text ClockTableFile

data ClockTableFile = ClockTableFile
  { clockTableFile :: Path Rel File,
    clockTableForest :: Forest ClockTableHeaderEntry
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON, ToYaml) via (Autodocodec ClockTableFile)

instance Validity ClockTableFile

instance HasCodec ClockTableFile where
  codec =
    object "ClockTableFile" $
      ClockTableFile
        <$> requiredField "file" "clock table file" .= clockTableFile
        <*> optionalFieldOrNullWithOmittedDefaultWith "forest" (entryForestCodec "ClockTableHeaderEntry" codec) [] "clock table forest" .= clockTableForest

data ClockTableHeaderEntry = ClockTableHeaderEntry
  { clockTableHeaderEntryHeader :: Header,
    clockTableHeaderEntryTime :: NominalDiffTime
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON, ToYaml) via (Autodocodec ClockTableHeaderEntry)

instance Validity ClockTableHeaderEntry

instance HasCodec ClockTableHeaderEntry where
  codec =
    object "ClockTableHeaderEntry" $
      ClockTableHeaderEntry
        <$> requiredField "header" "header" .= clockTableHeaderEntryHeader
        <*> requiredField "time" "how long was spent on the entry with the given header" .= clockTableHeaderEntryTime

-- Intermediary types
type ClockTimeBlock a = Block a FileTimes

data FileTimes = FileTimes
  { clockTimeFile :: Path Rel File,
    clockTimeForest :: TForest HeaderTimes
  }
  deriving (Generic)

data HeaderTimes f = HeaderTimes
  { headerTimesHeader :: Header,
    headerTimesEntries :: f LogbookEntry
  }
  deriving (Generic)

type TForest a = NonEmpty (TTree a)

data TTree a
  = TLeaf (a NonEmpty)
  | TBranch (a []) (TForest a)
  deriving (Generic)
