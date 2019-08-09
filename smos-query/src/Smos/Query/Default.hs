module Smos.Query.Default where

import Smos.Query

defaultSmosQuery :: IO ()
defaultSmosQuery = smosQuery defaultSmosQueryConfig

defaultSmosQueryConfig :: SmosQueryConfig
defaultSmosQueryConfig = SmosQueryConfig {smosQueryConfigReportConfig = defaultReportConfig}
