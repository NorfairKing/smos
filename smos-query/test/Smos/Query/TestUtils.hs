module Smos.Query.TestUtils where

import Control.Monad
import Data.Yaml as Yaml
import Path
import Path.IO
import Smos.Query
import Smos.Query.OptParse.Types as Query
import Smos.Report.InterestingStore
import System.Environment

testSmosQuery :: InterestingStore -> [String] -> IO ()
testSmosQuery = testSmosQueryWithConfig Query.defaultConfiguration

testSmosQueryWithConfig :: Query.Configuration -> InterestingStore -> [String] -> IO ()
testSmosQueryWithConfig configuration is args = withSystemTempDir "smos-query" $ \tdir -> do
  -- To make sure that my own homedir is not consulted for the config file when tests are run with stack, we have to clear the environment
  clearEnv
  configFile <- resolveFile tdir "config.yaml"
  Yaml.encodeFile (fromAbsFile configFile) configuration
  setEnv "SMOS_CONFIG_FILE" $ fromAbsFile configFile
  wd <- resolveDir tdir "workflow"
  setEnv "SMOS_WORKFLOW_DIR" $ fromAbsDir wd
  writeInterestingStore wd is
  withArgs args smosQuery

clearEnv :: IO ()
clearEnv = do
  envVars <- getEnvironment
  forM_ envVars $ \(key, _) ->
    unless (key == "TERM" || key == "TZ" || key == "TZDIR") $ unsetEnv key
