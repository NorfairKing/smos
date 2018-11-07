module Smos.Query.Default where

import Path

import Smos.Query.Config

smosQueryConfig :: Maybe (Path Abs Dir) -> SmosQueryConfig
smosQueryConfig Nothing =
    SmosQueryConfig {smosQueryConfigReportConfig = defaultReportConfig}
smosQueryConfig (Just path) =
    SmosQueryConfig
        { smosQueryConfigReportConfig =
              SmosReportConfig . AgendaFileSpec $ pure path
        }
