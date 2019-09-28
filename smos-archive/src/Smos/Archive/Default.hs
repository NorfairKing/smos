module Smos.Archive.Default where

import Smos.Archive

defaultSmosArchive :: IO ()
defaultSmosArchive = smosArchive defaultSmosArchiveConfig

defaultSmosArchiveConfig :: SmosArchiveConfig
defaultSmosArchiveConfig = SmosArchiveConfig {smosArchiveConfigReportConfig = defaultReportConfig}
