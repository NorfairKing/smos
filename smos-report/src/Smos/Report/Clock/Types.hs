{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Clock.Types where

import Autodocodec
import Autodocodec.Yaml.Encode
import Data.Aeson (ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Time
import Data.Yaml.Builder (ToYaml)
import Path
import Smos.Data
import Smos.Report.TimeBlock

-- Note: the order of these constructors matters
data TemporalClockResolution
  = TemporalSecondsResolution
  | TemporalMinutesResolution
  | TemporalHoursResolution
  deriving (Eq, Ord)

data DecimalClockResolution
  = DecimalHoursResolution
  | DecimalQuarterResolution
  | DecimalResolution Word -- Number of significant digits

data ClockFormat
  = ClockFormatTemporal TemporalClockResolution
  | ClockFormatDecimal DecimalClockResolution

data ClockReportStyle
  = ClockForest
  | ClockFlat

type ClockTable = [ClockTableBlock]

type ClockTableBlock = Block Text ClockTableFile

data ClockTableFile = ClockTableFile
  { clockTableFile :: Path Rel File,
    clockTableForest :: Forest ClockTableHeaderEntry
  }
  deriving (ToJSON) via (Autodocodec ClockTableFile)
  deriving (ToYaml) via (AutodocodecYaml ClockTableFile)

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
  deriving stock (Eq)

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

data HeaderTimes f = HeaderTimes
  { headerTimesHeader :: Header,
    headerTimesEntries :: f LogbookEntry
  }

type TForest a = NonEmpty (TTree a)

data TTree a
  = TLeaf (a NonEmpty)
  | TBranch (a []) (TForest a)
