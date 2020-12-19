module Smos.Archive.IntegrationSpec (spec) where

import Path
import Path.IO
import Smos.Archive
import Smos.Data
import System.Environment
import Test.Syd

spec :: Spec
spec = do
  let archive ls = withArgs ls smosArchive
  it "It 'just works' with a given workflow dir" $
    withSystemTempDir "smos-archive-test" $
      \ad -> do
        wd <- resolveDir ad "workflow"
        tf <- resolveFile wd "task"
        ensureDir $ parent tf
        writeSmosFile tf emptySmosFile
        archive [fromAbsFile tf, "--workflow-dir", fromAbsDir wd]
