module Smos.Single.OptParse.Types where

import Path

import Smos.Data

import Smos.Report.Config as Report
import Smos.Report.OptParse.Types as Report

data Flags =
  Flags
    { flagTaskPieces :: [String]
    , flagTaskFile :: Maybe FilePath
    , flagReportFlags :: Report.Flags
    }
  deriving (Show, Eq)

data Configuration =
  Configuration
    { confReportConfiguration :: Report.Configuration
    }
  deriving (Show, Eq)

data Environment =
  Environment
    { envReportEnvironment :: Report.Environment
    }
  deriving (Show, Eq)

data Settings =
  Settings
    { setTask :: Header
    , setTaskFile :: Maybe (Path Rel File)
    , setReportSettings :: SmosReportConfig
    }
  deriving (Show, Eq)
