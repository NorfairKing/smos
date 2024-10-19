module Smos.Directory.TestUtils where

import Path.IO
import Smos.Directory.InterestingStore
import Smos.Directory.OptParse
import Test.QuickCheck
import Test.Syd.Validity

withInterestingStore :: (DirectorySettings -> IO ()) -> Property
withInterestingStore func = forAllValid $ \is -> withDirectorySettings is func

withDirectorySettings :: InterestingStore -> (DirectorySettings -> IO a) -> IO a
withDirectorySettings is func =
  withSystemTempDir "smos-report-test" $ \tempDir -> do
    writeInterestingStore tempDir is
    let dc =
          defaultDirectorySettings
            { directoryConfigWorkflowFileSpec = AbsoluteWorkflow tempDir
            }
    func dc
