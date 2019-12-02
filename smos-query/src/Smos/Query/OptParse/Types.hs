{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.OptParse.Types
  ( module Smos.Report.Clock.Types
  , module Smos.Report.Agenda.Types
  , module Smos.Query.OptParse.Types
  , module Smos.Report.ShouldPrint
  ) where

import GHC.Generics (Generic)

import Data.List.NonEmpty (NonEmpty(..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Yaml as Yaml

import qualified Smos.Report.OptParse.Types as Report

import Smos.Data

import Smos.Report.Agenda.Types
import Smos.Report.Clock.Types
import Smos.Report.Filter
import Smos.Report.Period
import Smos.Report.Projection
import Smos.Report.ShouldPrint
import Smos.Report.Sorter
import Smos.Report.Time
import Smos.Report.TimeBlock

import Smos.Query.Config

data Arguments =
  Arguments Command Flags
  deriving (Show, Eq)

data Instructions =
  Instructions Dispatch SmosQueryConfig

data Command
  = CommandEntry EntryFlags
  | CommandWork WorkFlags
  | CommandWaiting WaitingFlags
  | CommandNext NextFlags
  | CommandClock ClockFlags
  | CommandAgenda AgendaFlags
  | CommandProjects ProjectsFlags
  | CommandLog LogFlags
  | CommandStats StatsFlags
  | CommandTags TagsFlags
  deriving (Show, Eq)

data EntryFlags =
  EntryFlags
    { entryFlagFilter :: Maybe EntryFilter
    , entryFlagProjection :: Maybe (NonEmpty Projection)
    , entryFlagSorter :: Maybe Sorter
    , entryFlagHideArchive :: Maybe HideArchive
    }
  deriving (Show, Eq)

data WorkFlags =
  WorkFlags
    { workFlagContext :: ContextName
    , workFlagTimeFilter :: Filter Time
    , workFlagFilter :: Maybe EntryFilter
    , workFlagProjection :: Maybe (NonEmpty Projection)
    , workFlagSorter :: Maybe Sorter
    , workFlagHideArchive :: Maybe HideArchive
    }
  deriving (Show, Eq)

data WaitingFlags =
  WaitingFlags
    { waitingFlagFilter :: Maybe EntryFilter
    , waitingFlagHideArchive :: Maybe HideArchive
    }
  deriving (Show, Eq)

data NextFlags =
  NextFlags
    { nextFlagFilter :: Maybe EntryFilter
    , nextFlagHideArchive :: Maybe HideArchive
    }
  deriving (Show, Eq)

data ClockFlags =
  ClockFlags
    { clockFlagFilter :: Maybe EntryFilter
    , clockFlagPeriodFlags :: Maybe Period
    , clockFlagBlockFlags :: Maybe TimeBlock
    , clockFlagOutputFormat :: Maybe OutputFormat
    , clockFlagClockFormat :: Maybe ClockFormatFlags
    , clockFlagReportStyle :: Maybe ClockReportStyle
    , clockFlagHideArchive :: Maybe HideArchive
    }
  deriving (Show, Eq)

data ClockFormatFlags
  = ClockFormatTemporalFlag (Maybe TemporalClockResolution)
  | ClockFormatDecimalFlag (Maybe DecimalClockResolution)
  deriving (Show, Eq)

data AgendaFlags =
  AgendaFlags
    { agendaFlagFilter :: Maybe EntryFilter
    , agendaFlagHistoricity :: Maybe AgendaHistoricity
    , agendaFlagBlock :: Maybe TimeBlock
    , agendaFlagHideArchive :: Maybe HideArchive
    }
  deriving (Show, Eq)

data ProjectsFlags =
  ProjectsFlags
    { projectsFlagFilter :: Maybe ProjectFilter
    }
  deriving (Show, Eq)

data LogFlags =
  LogFlags
    { logFlagFilter :: Maybe EntryFilter
    , logFlagPeriodFlags :: Maybe Period
    , logFlagBlockFlags :: Maybe TimeBlock
    , logFlagHideArchive :: Maybe HideArchive
    }
  deriving (Show, Eq, Generic)

newtype StatsFlags =
  StatsFlags
    { statsFlagPeriodFlags :: Maybe Period
    }
  deriving (Show, Eq, Generic)

newtype TagsFlags =
  TagsFlags
    { tagsFlagFilter :: Maybe EntryFilter
    }
  deriving (Show, Eq, Generic)

newtype Flags =
  Flags
    { flagReportFlags :: Report.Flags
    }
  deriving (Show, Eq, Generic)

data Environment =
  Environment
    { envReportEnvironment :: Report.Environment
    , envHideArchive :: Maybe HideArchive
    }
  deriving (Show, Eq, Generic)

data Configuration =
  Configuration
    { confReportConf :: Report.Configuration
    , confHideArchive :: Maybe HideArchive
    , confWorkConfiguration :: Maybe WorkConfiguration
    }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON v =
    flip (withObject "Configuration") v $ \o ->
      Configuration <$> parseJSON v <*> o .:? "hide-archive" <*> o .:? "work"

data WorkConfiguration =
  WorkConfiguration
    { workConfChecks :: Set EntryFilter
    , workConfTimeFilterProperty :: Maybe PropertyName
    , workConfProjection :: Maybe (NonEmpty Projection)
    , workConfSorter :: Maybe Sorter
    }
  deriving (Show, Eq, Generic)

instance FromJSON WorkConfiguration where
  parseJSON =
    withObject "WorkConfiguration" $ \o ->
      WorkConfiguration <$> o .:? "checks" .!= S.empty <*> o .:? "time-filter" <*> o .:? "columns" <*>
      o .:? "sorter"

data Dispatch
  = DispatchEntry EntrySettings
  | DispatchWork WorkSettings
  | DispatchWaiting WaitingSettings
  | DispatchNext NextSettings
  | DispatchClock ClockSettings
  | DispatchAgenda AgendaSettings
  | DispatchProjects ProjectsSettings
  | DispatchLog LogSettings
  | DispatchStats StatsSettings
  | DispatchTags TagsSettings
  deriving (Show, Eq, Generic)

data EntrySettings =
  EntrySettings
    { entrySetFilter :: Maybe EntryFilter
    , entrySetProjection :: NonEmpty Projection
    , entrySetSorter :: Maybe Sorter
    , entrySetHideArchive :: HideArchive
    }
  deriving (Show, Eq, Generic)

data WorkSettings =
  WorkSettings
    { workSetContext :: ContextName
    , workSetTimeFilter :: Maybe (Filter Entry)
    , workSetFilter :: Maybe EntryFilter
    , workSetChecks :: Set EntryFilter
    , workSetProjection :: NonEmpty Projection
    , workSetSorter :: Maybe Sorter
    , workSetHideArchive :: HideArchive
    }
  deriving (Show, Eq, Generic)

data WaitingSettings =
  WaitingSettings
    { waitingSetFilter :: Maybe EntryFilter
    , waitingSetHideArchive :: HideArchive
    }
  deriving (Show, Eq, Generic)

data NextSettings =
  NextSettings
    { nextSetFilter :: Maybe EntryFilter
    , nextSetHideArchive :: HideArchive
    }
  deriving (Show, Eq, Generic)

data ClockSettings =
  ClockSettings
    { clockSetFilter :: Maybe EntryFilter
    , clockSetPeriod :: Period
    , clockSetBlock :: TimeBlock
    , clockSetOutputFormat :: OutputFormat
    , clockSetClockFormat :: ClockFormat
    , clockSetReportStyle :: ClockReportStyle
    , clockSetHideArchive :: HideArchive
    }
  deriving (Show, Eq, Generic)

data AgendaSettings =
  AgendaSettings
    { agendaSetFilter :: Maybe EntryFilter
    , agendaSetHistoricity :: AgendaHistoricity
    , agendaSetBlock :: TimeBlock
    , agendaSetHideArchive :: HideArchive
    }
  deriving (Show, Eq, Generic)

data ProjectsSettings =
  ProjectsSettings
    { projectsSetFilter :: Maybe ProjectFilter
    }
  deriving (Show, Eq, Generic)

data LogSettings =
  LogSettings
    { logSetFilter :: Maybe EntryFilter
    , logSetPeriod :: Period
    , logSetBlock :: TimeBlock
    , logSetHideArchive :: HideArchive
    }
  deriving (Show, Eq, Generic)

newtype StatsSettings =
  StatsSettings
    { statsSetPeriod :: Period
    }
  deriving (Show, Eq, Generic)

newtype TagsSettings =
  TagsSettings
    { tagsSetFilter :: Maybe EntryFilter
    }
  deriving (Show, Eq, Generic)

data OutputFormat
  = OutputPretty
  | OutputYaml
  | OutputJSON
  | OutputJSONPretty
  deriving (Show, Eq, Generic)
