module Smos.Query.TestUtils where

import Control.Monad
import Path
import Path.IO
import Smos.Directory.InterestingStore
import Smos.Query
import System.Environment

testSmosQuery :: InterestingStore -> [String] -> IO ()
testSmosQuery is args = withSystemTempDir "smos-query" $ \tdir -> do
  -- To make sure that my own homedir is not consulted for the config file when tests are run with stack, we have to clear the environment
  clearEnv
  wd <- resolveDir tdir "workflow"
  setEnv "SMOS_WORKFLOW_DIR" $ fromAbsDir wd
  writeInterestingStore wd is
  withArgs args smosQuery

clearEnv :: IO ()
clearEnv = do
  envVars <- getEnvironment
  forM_ envVars $ \(key, _) ->
    unless (key == "TERM" || key == "TZ" || key == "TZDIR") $ unsetEnv key
