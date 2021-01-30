module Smos.Query.TestUtils where

import Path
import Path.IO
import Smos.Query
import Smos.Query.Default
import Smos.Report.InterestingStore
import System.Environment

testSmosQuery :: InterestingStore -> [String] -> IO ()
testSmosQuery = testSmosQueryWithConfig defaultSmosQueryConfig

testSmosQueryWithConfig :: SmosQueryConfig -> InterestingStore -> [String] -> IO ()
testSmosQueryWithConfig cfg is args = withSystemTempDir "smos-query" $ \tdir -> do
  wd <- resolveDir tdir "workflow"
  writeInterestingStore wd is
  withArgs args $ smosQuery $ setWorkflowDir wd cfg

setWorkflowDir :: Path Abs Dir -> SmosQueryConfig -> SmosQueryConfig
setWorkflowDir wd sqc =
  sqc
    { smosQueryConfigReportConfig =
        SmosReportConfig
          { smosReportConfigWorkConfig = defaultWorkReportConfig,
            smosReportConfigDirectoryConfig =
              defaultDirectoryConfig
                { directoryConfigWorkflowFileSpec = AbsoluteWorkflow wd
                }
          }
    }
