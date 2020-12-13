{-# LANGUAGE OverloadedStrings #-}

module Smos.Scheduler.IntegrationSpec
  ( spec,
  )
where

import Path
import Path.IO
import Smos.Report.Config
import Smos.Report.TestUtils
import Smos.Scheduler.Commands
import Smos.Scheduler.OptParse.Types
import Smos.Scheduler.Render.Gen ()
import Smos.Scheduler.Utils
import System.Cron.Types
import Test.Syd

import Test.QuickCheck
import Test.Syd.Validity hiding (check)

spec :: Spec
spec = modifyMaxSuccess (`div` 10) $ do
  it "'just works'" $
    forAllValid $ \templatePath ->
      forAll (genValid `suchThat` (/= templatePath)) $ \destinationPath ->
        withInterestingStore $ \dc ->
          withSystemTempDir "smos-scheduler-test" $ \td -> do
            let scheduleTemplate = ScheduleTemplate []
            wd <- resolveDirWorkflowDir dc
            writeYamlFile (wd </> templatePath) (scheduleTemplate :: ScheduleTemplate)
            sf <- resolveFile td "state-file"
            let sets =
                  Settings
                    { setDirectorySettings = dc,
                      setStateFile = sf,
                      setSchedule =
                        Schedule
                          [ ScheduleItem
                              { scheduleItemDescription = Just "Example",
                                scheduleItemTemplate = templatePath,
                                scheduleItemDestination = DestinationPathTemplate destinationPath,
                                scheduleItemCronSchedule = everyMinute -- Should definitely get activated
                              }
                          ]
                    }
            check sets -- The first check
            schedule sets -- The first run
            doesFileExist sf `shouldReturn` True -- There should be a state file now
            doesFileExist (wd </> destinationPath) `shouldReturn` True -- There should be a destination file
            check sets -- The second check
