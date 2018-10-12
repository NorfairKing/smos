module Smos.Query.OptParse.Types
    ( module Smos.Report.Clock.Types
    , module Smos.Report.Agenda.Types
    , module Smos.Query.OptParse.Types
    , module Smos.Report.ShouldPrint
    ) where

import Path

import Smos.Report.Agenda.Types
import Smos.Report.Clock.Types
import Smos.Report.Query
import Smos.Report.ShouldPrint
import Smos.Report.TimeBlock

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandEntry EntryFlags
    | CommandWaiting WaitingFlags
    | CommandNext NextFlags
    | CommandClock ClockFlags
    | CommandAgenda AgendaFlags
    deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data EntryFlags = EntryFlags
    { entryFlagFilter :: Maybe Filter
    } deriving (Show, Eq)

data WaitingFlags = WaitingFlags
    { waitingFlagFilter :: Maybe Filter
    } deriving (Show, Eq)

data NextFlags = NextFlags
    { nextFlagFilter :: Maybe Filter
    } deriving (Show, Eq)

data ClockFlags = ClockFlags
    { clockFlagFile :: Maybe FilePath
    , clockFlagFilter :: Maybe Filter
    , clockFlagPeriodFlags :: Maybe ClockPeriod
    , clockFlagResolutionFlags :: Maybe ClockResolution
    , clockFlagBlockFlags :: Maybe TimeBlock
    } deriving (Show, Eq)

data AgendaFlags = AgendaFlags
    { agendaFlagFilter :: Maybe Filter
    , agendaFlagHistoricity :: Maybe AgendaHistoricity
    , agendaFlagBlock :: Maybe TimeBlock
    } deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)

data Dispatch
    = DispatchEntry EntrySettings
    | DispatchWaiting WaitingSettings
    | DispatchNext NextSettings
    | DispatchClock ClockSettings
    | DispatchAgenda AgendaSettings
    deriving (Show, Eq)

data EntrySettings = EntrySettings
    { entrySetFilter :: Maybe Filter
    } deriving (Show, Eq)

data WaitingSettings = WaitingSettings
    { waitingSetFilter :: Maybe Filter
    } deriving (Show, Eq)

data NextSettings = NextSettings
    { nextSetFilter :: Maybe Filter
    } deriving (Show, Eq)

data ClockSettings = ClockSettings
    { clockSetFile :: Maybe (Path Abs File)
    , clockSetFilter :: Maybe Filter
    , clockSetPeriod :: ClockPeriod
    , clockSetResolution :: ClockResolution
    , clockSetBlock :: TimeBlock
    } deriving (Show, Eq)

data AgendaSettings = AgendaSettings
    { agendaSetFilter :: Maybe Filter
    , agendaSetHistoricity :: AgendaHistoricity
    , agendaSetBlock :: TimeBlock
    } deriving (Show, Eq)
