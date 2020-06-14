module Smos.Query.TestUtils where

import Path
import Path.IO
import Smos.Query
import Smos.Report.InterestingStore
import System.Environment

testSmosQuery :: InterestingStore -> [String] -> IO ()
testSmosQuery is args = withSystemTempDir "smos-query" $ \tdir -> do
  wd <- resolveDir tdir "workflow"
  writeInterestingStore wd is
  withArgs args $ smosQuery (testConfig wd)

testConfig :: Path Abs Dir -> SmosQueryConfig
testConfig td =
  SmosQueryConfig
    { smosQueryConfigReportConfig =
        SmosReportConfig
          { smosReportConfigWorkConfig = defaultWorkReportConfig,
            smosReportConfigDirectoryConfig =
              defaultDirectoryConfig
                { directoryConfigWorkflowFileSpec = DirAbsolute td
                }
          }
    }
