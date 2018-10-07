module Smos.Query.OptParse.Types
    ( module Smos.Report.Clock.Types
    , module Smos.Report.Agenda.Types
    , module Smos.Query.OptParse.Types
    , module Smos.Report.ShouldPrint
    ) where

import Smos.Data

import Smos.Report.Agenda.Types
import Smos.Report.Clock.Types
import Smos.Report.ShouldPrint

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandWaiting
    | CommandNext
    | CommandClock ClockFlags
    | CommandAgenda AgendaFlags
    deriving (Show, Eq)

data Flags = Flags
    deriving (Show, Eq)

data ClockFlags = ClockFlags
    { clockFlagPeriodFlags :: Maybe ClockPeriod
    , clockFlagResolutionFlags :: Maybe ClockResolution
    , clockFlagBlockFlags :: Maybe ClockBlock
    , clockFlagTags :: [Tag]
    } deriving (Show, Eq)

data AgendaFlags = AgendaFlags
    { agendaFlagHistoricity :: Maybe AgendaHistoricity
    } deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)

data Dispatch
    = DispatchWaiting
    | DispatchNext
    | DispatchClock ClockSettings
    | DispatchAgenda AgendaSettings
    deriving (Show, Eq)

data ClockSettings = ClockSettings
    { clockSetPeriod :: ClockPeriod
    , clockSetResolution :: ClockResolution
    , clockSetBlock :: ClockBlock
    , clockSetTags :: [Tag]
    } deriving (Show, Eq)

data AgendaSettings = AgendaSettings
    { agendaSetHistoricity :: AgendaHistoricity
    } deriving (Show, Eq)
