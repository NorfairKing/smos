{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Config
    ( SmosReportConfig(..)
    , defaultReportConfig
    , AgendaFileSpec(..)
    , inHomeDir
    ) where

import GHC.Generics (Generic)

import Path
import Path.IO

data SmosReportConfig = SmosReportConfig
    { smosReportConfigAgendaFileSpec :: AgendaFileSpec
    } deriving (Generic)

defaultReportConfig :: SmosReportConfig
defaultReportConfig =
    SmosReportConfig {smosReportConfigAgendaFileSpec = inHomeDir "workflow"}

data AgendaFileSpec = AgendaFileSpec
    { agendaFileSpecGetWorkDir :: IO (Path Abs Dir)
    } deriving (Generic)

inHomeDir :: FilePath -> AgendaFileSpec
inHomeDir fp =
    AgendaFileSpec $ do
        home <- getHomeDir
        resolveDir home fp
