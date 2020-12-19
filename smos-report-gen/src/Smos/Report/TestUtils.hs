module Smos.Report.TestUtils where

import Path.IO
import Smos.Report.Config
import Smos.Report.InterestingStore
import Test.QuickCheck
import Test.Syd.Validity

withInterestingStore :: (DirectoryConfig -> IO ()) -> Property
withInterestingStore func = forAllValid $ \is -> withDirectoryConfig is func

withDirectoryConfig :: InterestingStore -> (DirectoryConfig -> IO a) -> IO a
withDirectoryConfig is func =
  withSystemTempDir "smos-report-test" $ \tempDir -> do
    writeInterestingStore tempDir is
    let dc = defaultDirectoryConfig {directoryConfigWorkflowFileSpec = AbsoluteWorkflow tempDir}
    func dc
