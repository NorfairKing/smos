{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Clock.Types where

import Control.Applicative
import Data.Aeson
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Time
import Data.Validity
import Data.Validity.Path ()
import Data.Yaml.Builder (ToYaml (..))
import qualified Data.Yaml.Builder as Yaml
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
  deriving (Show, Eq, Generic)

instance Validity ClockTableFile

instance ToJSON ClockTableFile where
  toJSON ClockTableFile {..} =
    object ["file" .= clockTableFile, "forest" .= ForYaml clockTableForest]

instance ToYaml ClockTableFile where
  toYaml ClockTableFile {..} =
    Yaml.mapping [("file", toYaml clockTableFile), ("forest", toYaml $ ForYaml clockTableForest)]

instance FromJSON (ForYaml (Tree ClockTableHeaderEntry)) where
  parseJSON v =
    ForYaml
      <$> ( ( withObject "Tree ClockTableHeaderEntry" $ \o ->
                Node <$> o .: "entry" <*> (unForYaml <$> o .:? "forest" .!= ForYaml [])
            )
              v
              <|> (Node <$> parseJSON v <*> pure [])
          )

instance ToJSON (ForYaml (Tree ClockTableHeaderEntry)) where
  toJSON (ForYaml Node {..}) =
    if null subForest
      then toJSON rootLabel
      else object ["entry" .= rootLabel, "forest" .= ForYaml subForest]

instance ToYaml (ForYaml (Tree ClockTableHeaderEntry)) where
  toYaml (ForYaml Node {..}) =
    if null subForest
      then toYaml rootLabel
      else Yaml.mapping [("entry", toYaml rootLabel), ("forest", toYaml (ForYaml subForest))]

data ClockTableHeaderEntry = ClockTableHeaderEntry
  { clockTableHeaderEntryHeader :: Header,
    clockTableHeaderEntryTime :: NominalDiffTime
  }
  deriving (Show, Eq, Generic)

instance Validity ClockTableHeaderEntry

instance FromJSON ClockTableHeaderEntry where
  parseJSON =
    withObject "ClockTableHeaderEntry" $ \o ->
      ClockTableHeaderEntry <$> o .: "header" <*> o .: "time"

instance ToJSON ClockTableHeaderEntry where
  toJSON ClockTableHeaderEntry {..} =
    object ["header" .= clockTableHeaderEntryHeader, "time" .= clockTableHeaderEntryTime]

instance ToYaml ClockTableHeaderEntry where
  toYaml ClockTableHeaderEntry {..} =
    Yaml.mapping
      [("header", toYaml clockTableHeaderEntryHeader), ("time", toYaml clockTableHeaderEntryTime)]

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
