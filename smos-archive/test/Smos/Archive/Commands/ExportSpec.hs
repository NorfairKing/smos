{-# LANGUAGE QuasiQuotes #-}

module Smos.Archive.Commands.ExportSpec
  ( spec,
  )
where

import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.DirForest as DF
import Data.Time
import Path
import Path.IO
import Smos.Archive.Commands.Export
import Smos.Archive.Commands.File
import Smos.Archive.Env
import Smos.Archive.OptParse
import Smos.Data
import Smos.Data.Gen ()
import Smos.Directory.InterestingStore
import Smos.Directory.OptParse.Types
import Smos.Directory.Resolution
import Smos.Report.Filter
import Smos.Report.Period
import Test.Syd
import Test.Syd.Path
import Test.Syd.Validity

spec :: Spec
spec = modifyMaxShrinks (const 0) $
  modifyMaxSuccess (`div` 10) $
    tempDirSpec "smos-archive" $
      describe "prepareToArchive" $ do
        logFunc <- liftIO $ runNoLoggingT askLoggerIO
        let testA settings aFunc = runLoggingT (runA settings aFunc) logFunc
        it "produces the same example workdir" $ \tdir -> do
          workflowDir <- resolveDir' "test_resources/example-workflow"
          expectedExportDir <- resolveDir' "test_resources/example-export"
          actualExportDir <- resolveDir tdir "export"
          let settings =
                Settings
                  { setDirectorySettings =
                      defaultDirectorySettings
                        { directoryConfigWorkflowFileSpec = AbsoluteWorkflow workflowDir
                        },
                    setLogLevel = LevelWarn
                  }
          let exportSettings =
                ExportSettings
                  { exportSetExportDir = actualExportDir,
                    exportSetPeriod =
                      Just $
                        BeginEnd
                          (fromGregorian 2019 01 01)
                          (fromGregorian 2020 01 01),
                    exportSetFilter = Just $ FilterFile [relfile|client|],
                    exportSetAlsoDeleteOriginals = False
                  }
          testA settings (smosArchiveExport exportSettings)

          expected <- DF.read expectedExportDir (SB.readFile . fromAbsFile)
          actual <- DF.read actualExportDir (SB.readFile . fromAbsFile)
          actual `shouldBe` expected

        it "can export a file that it just archived" $ \tdir -> do
          workflowDir <- resolveDir tdir "workflow"
          exampleProjectFile <- resolveFile workflowDir "projects/example.smos"
          -- Empty smos file because then there is definitely no prompt about not-done entries
          writeSmosFile exampleProjectFile emptySmosFile
          let dc =
                defaultDirectorySettings
                  { directoryConfigWorkflowFileSpec = AbsoluteWorkflow workflowDir
                  }
              settings =
                Settings
                  { setDirectorySettings = dc,
                    setLogLevel = LevelWarn
                  }

          archiveDir <- resolveDirArchiveDir dc

          testA settings (smosArchiveFile exampleProjectFile)

          actualExportDir <- resolveDir tdir "export"
          let exportSettings =
                ExportSettings
                  { exportSetExportDir = actualExportDir,
                    exportSetPeriod = Nothing,
                    exportSetFilter = Nothing,
                    exportSetAlsoDeleteOriginals = False
                  }

          testA settings (smosArchiveExport exportSettings)

          expected <- DF.read archiveDir (SB.readFile . fromAbsFile)
          actual <- DF.read actualExportDir (SB.readFile . fromAbsFile)

          actual `shouldBe` expected

        it "does not crash, use interesting store" $ \tdir ->
          forAllValid $ \is -> do
            workflowDir <- resolveDir tdir "workflow"
            writeInterestingStore workflowDir is
            let settings =
                  Settings
                    { setDirectorySettings =
                        defaultDirectorySettings
                          { directoryConfigWorkflowFileSpec = AbsoluteWorkflow workflowDir
                          },
                      setLogLevel = LevelWarn
                    }
            actualExportDir <- resolveDir tdir "export"
            let exportSettings =
                  ExportSettings
                    { exportSetExportDir = actualExportDir,
                      exportSetPeriod = Nothing,
                      exportSetFilter = Nothing,
                      exportSetAlsoDeleteOriginals = True
                    }

            testA settings (smosArchiveExport exportSettings)
