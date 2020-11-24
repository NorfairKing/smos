module Smos.Query.TestUtils where

import Path
import Path.IO
import Smos.Query
import Smos.Query.Default
import Smos.Report.InterestingStore
import System.Environment

testSmosQuery :: InterestingStore -> [String] -> IO ()
testSmosQuery is args = withSystemTempDir "smos-query" $ \tdir -> do
  wd <- resolveDir tdir "workflow"
  writeInterestingStore wd is
  withArgs args $ smosQuery (testConfig wd)

testConfig :: Path Abs Dir -> SmosQueryConfig
testConfig td =
  defaultSmosQueryConfig
    { smosQueryConfigReportConfig =
        SmosReportConfig
          { smosReportConfigWorkConfig = defaultWorkReportConfig,
            smosReportConfigDirectoryConfig =
              defaultDirectoryConfig
                { directoryConfigWorkflowFileSpec = AbsoluteWorkflow td
                }
          }
    }
