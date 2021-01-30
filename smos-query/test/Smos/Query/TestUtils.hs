module Smos.Query.TestUtils where

import Control.Monad
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
  -- To make sure that my own homedir is not consulted for the config file when tests are run with stack, we have to clear the environment
  clearEnv
  cd <- resolveDir tdir "configdir"
  setEnv "XDG_CONFIG_HOME" $ fromAbsDir cd
  wd <- resolveDir tdir "workflow"
  writeInterestingStore wd is
  let sqc = setWorkflowDir wd cfg
  withArgs args $ smosQuery sqc

clearEnv :: IO ()
clearEnv = do
  envVars <- getEnvironment
  forM_ envVars $ \(key, _) -> unless (key == "TERM") $ unsetEnv key

setWorkflowDir :: Path Abs Dir -> SmosQueryConfig -> SmosQueryConfig
setWorkflowDir wd sqc =
  sqc
    { smosQueryConfigReportConfig =
        (smosQueryConfigReportConfig sqc)
          { smosReportConfigDirectoryConfig =
              ( smosReportConfigDirectoryConfig
                  (smosQueryConfigReportConfig sqc)
              )
                { directoryConfigWorkflowFileSpec = AbsoluteWorkflow wd
                }
          }
    }
