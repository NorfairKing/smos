{-# LANGUAGE NoImplicitPrelude #-}

module Smos.Report.OptParse.Types where

import Import

import Data.Configurator.Types
import qualified Data.Text as T

data ShouldPrint
    = PrintError
    | PrintWarning
    | DontPrint
    deriving (Show, Eq)

parseShouldPrint :: String -> Maybe ShouldPrint
parseShouldPrint "error" = Just PrintError
parseShouldPrint "warning" = Just PrintWarning
parseShouldPrint "nothing" = Just DontPrint
parseShouldPrint _ = Nothing

instance Configured ShouldPrint where
    convert (String t) = parseShouldPrint $ T.unpack t
    convert _ = Nothing

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandWaiting
    | CommandNext
    deriving (Show, Eq)

data Flags = Flags
    { flagConfigFile :: Maybe FilePath
    , flagWorkDir :: Maybe FilePath
    , flagShouldPrint :: Maybe ShouldPrint
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
    deriving (Show, Eq)
