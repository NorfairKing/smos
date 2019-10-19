module Smos.Scheduler.OptParse.Types where

import Path

import Smos.Data

import Smos.Report.Config as Report
import Smos.Report.OptParse.Types as Report

newtype Flags =
  Flags
    { flagReportFlags :: Report.Flags
    }
  deriving (Show, Eq)

newtype Configuration =
  Configuration
    { confReportConfiguration :: Report.Configuration
    }
  deriving (Show, Eq)

newtype Environment =
  Environment
    { envReportEnvironment :: Report.Environment
    }
  deriving (Show, Eq)

newtype Settings =
  Settings
    { setReportSettings :: SmosReportConfig
    }
  deriving (Show, Eq)
