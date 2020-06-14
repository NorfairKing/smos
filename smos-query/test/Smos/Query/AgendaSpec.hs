module Smos.Query.AgendaSpec (spec) where

import Path
import Path.IO
import Smos.Query
import Smos.Report.InterestingStore
import System.Environment
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity

spec :: Spec
spec = modifyMaxSuccess (`div` 50) -- The first test will be empty, the second will not
  $ describe "Agenda"
  $ it "'just works' for any InterestingStore"
  $ forAllValid
  $ \is -> testSmosQuery is ["agenda"]

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
