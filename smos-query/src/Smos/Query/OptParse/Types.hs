{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.OptParse.Types
  ( module Smos.Report.Clock.Types
  , module Smos.Report.Agenda.Types
  , module Smos.Query.OptParse.Types
  , module Smos.Report.ShouldPrint
  ) where

import GHC.Generics (Generic)

import Path

import Data.Yaml as Yaml

import qualified Smos.Report.OptParse.Types as Report

import Smos.Report.Agenda.Types
import Smos.Report.Clock.Types
import Smos.Report.Filter
import Smos.Report.Period
import Smos.Report.Projection
import Smos.Report.ShouldPrint
import Smos.Report.Sorter
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
  | CommandProjects
  | CommandLog LogFlags
  | CommandStats StatsFlags
  | CommandTags TagsFlags
  deriving (Show, Eq)

data EntryFlags =
  EntryFlags
    { entryFlagFilter :: Maybe Filter
    , entryFlagProjection :: Maybe Projection
    , entryFlagSorter :: Maybe Sorter
    }
  deriving (Show, Eq)

data WorkFlags =
  WorkFlags
    { workFlagContext :: ContextName
    , workFlagFilter :: Maybe Filter
    }
  deriving (Show, Eq)

data WaitingFlags =
  WaitingFlags
    { waitingFlagFilter :: Maybe Filter
    }
  deriving (Show, Eq)

data NextFlags =
  NextFlags
    { nextFlagFilter :: Maybe Filter
    }
  deriving (Show, Eq)

data ClockFlags =
  ClockFlags
    { clockFlagFile :: Maybe FilePath
    , clockFlagFilter :: Maybe Filter
    , clockFlagPeriodFlags :: Maybe Period
    , clockFlagResolutionFlags :: Maybe ClockResolution
    , clockFlagBlockFlags :: Maybe TimeBlock
    , clockFlagOutputFormat :: Maybe OutputFormat
    , clockFlagReportStyle :: Maybe ClockReportStyle
    }
  deriving (Show, Eq)

data AgendaFlags =
  AgendaFlags
    { agendaFlagFilter :: Maybe Filter
    , agendaFlagHistoricity :: Maybe AgendaHistoricity
    , agendaFlagBlock :: Maybe TimeBlock
    }
  deriving (Show, Eq)

data LogFlags =
  LogFlags
    { logFlagFilter :: Maybe Filter
    , logFlagPeriodFlags :: Maybe Period
    , logFlagBlockFlags :: Maybe TimeBlock
    }
  deriving (Show, Eq, Generic)

data StatsFlags =
  StatsFlags
    { statsFlagPeriodFlags :: Maybe Period
    }
  deriving (Show, Eq, Generic)

data TagsFlags =
  TagsFlags
    { tagsFlagFilter :: Maybe Filter
    }
  deriving (Show, Eq, Generic)

data Flags =
  Flags
    { flagReportFlags :: Report.Flags
    , flagHideArchive :: Maybe HideArchive
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
    }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON v =
    flip (withObject "Configuration") v $ \o ->
      Configuration <$> parseJSON v <*> o .:? "hide-archive"

data Dispatch
  = DispatchEntry EntrySettings
  | DispatchWork WorkSettings
  | DispatchWaiting WaitingSettings
  | DispatchNext NextSettings
  | DispatchClock ClockSettings
  | DispatchAgenda AgendaSettings
  | DispatchProjects
  | DispatchLog LogSettings
  | DispatchStats StatsSettings
  | DispatchTags TagsSettings
  deriving (Show, Eq, Generic)

data EntrySettings =
  EntrySettings
    { entrySetFilter :: Maybe Filter
    , entrySetProjection :: Maybe Projection
    , entrySetSorter :: Maybe Sorter
    }
  deriving (Show, Eq, Generic)

data WorkSettings =
  WorkSettings
    { workSetContext :: ContextName
    , workSetFilter :: Maybe Filter
    }
  deriving (Show, Eq, Generic)

data WaitingSettings =
  WaitingSettings
    { waitingSetFilter :: Maybe Filter
    }
  deriving (Show, Eq, Generic)

data NextSettings =
  NextSettings
    { nextSetFilter :: Maybe Filter
    }
  deriving (Show, Eq, Generic)

data ClockSettings =
  ClockSettings
    { clockSetFile :: Maybe (Path Abs File)
    , clockSetFilter :: Maybe Filter
    , clockSetPeriod :: Period
    , clockSetResolution :: ClockResolution
    , clockSetBlock :: TimeBlock
    , clockSetOutputFormat :: OutputFormat
    , clockSetReportStyle :: ClockReportStyle
    }
  deriving (Show, Eq, Generic)

data AgendaSettings =
  AgendaSettings
    { agendaSetFilter :: Maybe Filter
    , agendaSetHistoricity :: AgendaHistoricity
    , agendaSetBlock :: TimeBlock
    }
  deriving (Show, Eq, Generic)

data LogSettings =
  LogSettings
    { logSetFilter :: Maybe Filter
    , logSetPeriod :: Period
    , logSetBlock :: TimeBlock
    }
  deriving (Show, Eq, Generic)

data StatsSettings =
  StatsSettings
    { statsSetPeriod :: Period
    }
  deriving (Show, Eq, Generic)

data TagsSettings =
  TagsSettings
    { tagsSetFilter :: Maybe Filter
    }
  deriving (Show, Eq, Generic)

data OutputFormat
  = OutputPretty
  | OutputYaml
  | OutputJSON
  | OutputJSONPretty
  deriving (Show, Eq, Generic)
