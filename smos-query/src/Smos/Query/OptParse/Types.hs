module Smos.Query.OptParse.Types
    ( module Smos.Report.Clock.Types
    , module Smos.Report.Agenda.Types
    , module Smos.Query.OptParse.Types
    , module Smos.Report.ShouldPrint
    ) where

import Path

import Data.Yaml as Yaml

import qualified Smos.Report.OptParse.Types as Report

import Smos.Report.Agenda.Types
import Smos.Report.Clock.Types
import Smos.Report.Period
import Smos.Report.Projection
import Smos.Report.Query
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
    | CommandWaiting WaitingFlags
    | CommandNext NextFlags
    | CommandClock ClockFlags
    | CommandAgenda AgendaFlags
    | CommandProjects
    | CommandLog LogFlags
    | CommandStats StatsFlags
    deriving (Show, Eq)

data EntryFlags =
    EntryFlags
        { entryFlagFilter :: Maybe Filter
        , entryFlagProjection :: Maybe Projection
        , entryFlagSorter :: Maybe Sorter
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
    deriving (Show, Eq)

data StatsFlags =
    StatsFlags
        { statsFlagFilter :: Maybe Filter
        , statsFlagPeriodFlags :: Maybe Period
        }
    deriving (Show, Eq)

data Flags =
    Flags
        { flagConfigFile :: Maybe FilePath
        , flagReportFlags :: Report.Flags
        }
    deriving (Show, Eq)

data Environment =
    Environment
        { envConfigFile :: Maybe FilePath
        , envReportEnv :: Report.Environment
        }
    deriving (Show, Eq)

data Configuration =
    Configuration
        { confReportConf :: Report.Configuration
        }
    deriving (Show, Eq)

instance FromJSON Configuration where
    parseJSON v = Configuration <$> parseJSON v

data Dispatch
    = DispatchEntry EntrySettings
    | DispatchWaiting WaitingSettings
    | DispatchNext NextSettings
    | DispatchClock ClockSettings
    | DispatchAgenda AgendaSettings
    | DispatchProjects
    | DispatchLog LogSettings
    | DispatchStats StatsSettings
    deriving (Show, Eq)

data EntrySettings =
    EntrySettings
        { entrySetFilter :: Maybe Filter
        , entrySetProjection :: Maybe Projection
        , entrySetSorter :: Maybe Sorter
        }
    deriving (Show, Eq)

data WaitingSettings =
    WaitingSettings
        { waitingSetFilter :: Maybe Filter
        }
    deriving (Show, Eq)

data NextSettings =
    NextSettings
        { nextSetFilter :: Maybe Filter
        }
    deriving (Show, Eq)

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
    deriving (Show, Eq)

data AgendaSettings =
    AgendaSettings
        { agendaSetFilter :: Maybe Filter
        , agendaSetHistoricity :: AgendaHistoricity
        , agendaSetBlock :: TimeBlock
        }
    deriving (Show, Eq)

data LogSettings =
    LogSettings
        { logSetFilter :: Maybe Filter
        , logSetPeriod :: Period
        , logSetBlock :: TimeBlock
        }
    deriving (Show, Eq)

data StatsSettings =
    StatsSettings
        { statsSetFilter :: Maybe Filter
        , statsSetPeriod :: Period
        }
    deriving (Show, Eq)

data OutputFormat
    = OutputPretty
    | OutputYaml
    | OutputJSON
    | OutputJSONPretty
    deriving (Show, Eq)
