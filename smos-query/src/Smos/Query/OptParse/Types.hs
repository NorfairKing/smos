module Smos.Query.OptParse.Types
    ( module Smos.Report.Clock.Types
    , module Smos.Report.Agenda.Types
    , module Smos.Query.OptParse.Types
    , module Smos.Report.ShouldPrint
    ) where

import Path

import Smos.Data

import Smos.Report.Agenda.Types
import Smos.Report.Clock.Types
import Smos.Report.TimeBlock
import Smos.Report.ShouldPrint

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandWaiting
    | CommandNext NextFlags
    | CommandClock ClockFlags
    | CommandAgenda AgendaFlags
    deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)


data NextFlags = NextFlags
    { nextFlagTags :: [Tag]
    } deriving (Show, Eq)

data ClockFlags = ClockFlags
    { clockFlagFile :: Maybe FilePath
    , clockFlagPeriodFlags :: Maybe ClockPeriod
    , clockFlagResolutionFlags :: Maybe ClockResolution
    , clockFlagBlockFlags :: Maybe TimeBlock
    , clockFlagTags :: [Tag]
    } deriving (Show, Eq)

data AgendaFlags = AgendaFlags
    { agendaFlagHistoricity :: Maybe AgendaHistoricity
    , agendaFlagBlock :: Maybe TimeBlock
    } deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)

data Dispatch
    = DispatchWaiting
    | DispatchNext NextSettings
    | DispatchClock ClockSettings
    | DispatchAgenda AgendaSettings
    deriving (Show, Eq)

data NextSettings = NextSettings
    { nextSetTags :: [Tag]
    } deriving (Show, Eq)

data ClockSettings = ClockSettings
    { clockSetFile :: Maybe (Path Abs File)
    , clockSetPeriod :: ClockPeriod
    , clockSetResolution :: ClockResolution
    , clockSetBlock :: TimeBlock
    , clockSetTags :: [Tag]
    } deriving (Show, Eq)

data AgendaSettings = AgendaSettings
    { agendaSetHistoricity :: AgendaHistoricity
    , agendaSetBlock :: TimeBlock
    } deriving (Show, Eq)
