module Smos.Query.Default where

import Smos.Query
import System.IO

defaultSmosQuery :: IO ()
defaultSmosQuery = smosQuery defaultSmosQueryConfig

defaultSmosQueryConfig :: SmosQueryConfig
defaultSmosQueryConfig =
  SmosQueryConfig
    { smosQueryConfigReportConfig = defaultReportConfig,
      smosQueryConfigColourConfig = defaultColourConfig,
      smosQueryConfigInputHandle = stdin,
      smosQueryConfigOutputHandle = stdout,
      smosQueryConfigErrorHandle = stderr
    }

defaultColourConfig :: ColourConfig
defaultColourConfig =
  ColourConfig
    { colourConfigBicolour = Nothing
    }
