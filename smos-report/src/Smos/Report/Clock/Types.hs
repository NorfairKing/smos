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
import OptEnvConf
import Path
import Smos.Data
import Smos.Report.TimeBlock

-- Note: the order of these constructors matters
data TemporalClockResolution
  = TemporalSecondsResolution
  | TemporalMinutesResolution
  | TemporalHoursResolution
  deriving (Eq, Ord)

instance HasParser TemporalClockResolution where
  settingsParser =
    choice
      [ setting
          [ help "Show clocks in seconds",
            switch TemporalSecondsResolution,
            long "seconds"
          ],
        setting
          [ help "Show clocks in minutes",
            switch TemporalMinutesResolution,
            long "minutes"
          ],
        setting
          [ help "Show clocks in hours",
            switch TemporalHoursResolution,
            long "hours"
          ]
      ]

data DecimalClockResolution
  = DecimalHoursResolution
  | DecimalQuarterResolution
  | DecimalResolution Word -- Number of significant digits

instance HasParser DecimalClockResolution where
  settingsParser =
    choice
      [ setting
          [ help "Show clocks in a decimal number of hours",
            switch DecimalHoursResolution,
            long "hours"
          ],
        setting
          [ help "Show clocks in a decimal number of quarter hours",
            switch DecimalQuarterResolution,
            long "quarters"
          ],
        setting
          [ help "Show clocks in a decimal manner, with this many decimals",
            option,
            reader $ DecimalResolution <$> auto,
            long "decimals",
            metavar "DIGITS"
          ]
      ]

data ClockFormat
  = ClockFormatTemporal TemporalClockResolution
  | ClockFormatDecimal DecimalClockResolution

instance HasParser ClockFormat where
  settingsParser =
    withShownDefault (ClockFormatTemporal TemporalMinutesResolution) "minutes" $
      choice
        [ setting
            [ help "Show the clocks with a temporal resolution (hours and minutes)",
              switch (),
              long "temporal"
            ]
            *> (ClockFormatTemporal <$> withShownDefault TemporalMinutesResolution "minutes" settingsParser),
          setting
            [ help "Show the clocks with a decimal resolution (hours and tenths of hours)",
              switch (),
              long "decimal"
            ]
            *> (ClockFormatDecimal <$> withShownDefault (DecimalResolution 2) "2 digits" settingsParser)
        ]

data ClockReportStyle
  = ClockForest
  | ClockFlat

instance HasParser ClockReportStyle where
  settingsParser =
    choice
      [ setting
          [ help "Show the clocks as a forest",
            switch ClockForest,
            long "forest"
          ],
        setting
          [ help "Show the clocks line by line",
            switch ClockFlat,
            long "flat"
          ]
      ]

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
