module Smos.Single.Default where

import Smos.Single

defaultSmosSingle :: IO ()
defaultSmosSingle = smosSingle defaultSmosSingleConfig

defaultSmosSingleConfig :: SmosSingleConfig
defaultSmosSingleConfig = SmosSingleConfig {smosSingleConfigReportConfig = defaultReportConfig}
