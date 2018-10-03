module Smos.Report.OptParse.Types
    ( module Smos.Report.OptParse.Types
    , module Smos.Report.ShouldPrint
    ) where

import Path

import Smos.Report.ShouldPrint

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandWaiting
    | CommandNext
    | CommandClock ClockFlags
    deriving (Show, Eq)

data Flags = Flags
    { flagConfigFile :: Maybe FilePath
    , flagWorkDir :: Maybe FilePath
    , flagShouldPrint :: Maybe ShouldPrint
    } deriving (Show, Eq)

data ClockFlags = ClockFlags
    { clockFlagToday :: Bool
    } deriving (Show, Eq)

data Configuration = Configuration
    { configWorkDir :: Maybe FilePath
    , configShouldPrint :: Maybe ShouldPrint
    } deriving (Show, Eq)

data Settings = Settings
    { setWorkDir :: Path Abs Dir
    , setShouldPrint :: ShouldPrint
    } deriving (Show, Eq)

data Dispatch
    = DispatchWaiting
    | DispatchNext
    | DispatchClock ClockSettings
    deriving (Show, Eq)

data ClockSettings = ClockSettings
    { clockSetPeriod :: ClockPeriod
    } deriving (Show, Eq)

data ClockPeriod
    = Today
    | AllTime
    deriving (Show, Eq)
