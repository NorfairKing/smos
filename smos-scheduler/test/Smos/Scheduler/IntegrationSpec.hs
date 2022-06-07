{-# LANGUAGE OverloadedStrings #-}

module Smos.Scheduler.IntegrationSpec
  ( spec,
  )
where

import Path
import Path.IO
import Smos.Query.OptParse (defaultColourSettings)
import Smos.Report.Config
import Smos.Report.TestUtils
import Smos.Scheduler.Commands
import Smos.Scheduler.OptParse.Types
import Smos.Scheduler.Render.Gen ()
import Smos.Scheduler.Utils
import System.Cron.Types
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity hiding (check)

spec :: Spec
spec = modifyMaxSuccess (`div` 10) $ do
  it "'just works'" $
    forAllValid $ \rentTemplatePath ->
      forAll (genValid `suchThat` (/= rentTemplatePath)) $ \haircutTemplatePath ->
        forAll (genValid `suchThat` (`notElem` [rentTemplatePath, haircutTemplatePath])) $ \rentDestinationPath ->
          forAll (genValid `suchThat` (`notElem` [rentTemplatePath, haircutTemplatePath, rentDestinationPath])) $ \haircutDestinationPath ->
            withInterestingStore $ \dc ->
              withSystemTempDir "smos-scheduler-test" $ \td -> do
                let rentScheduleTemplate = ScheduleTemplate []
                let haircutScheduleTemplate = ScheduleTemplate []
                wd <- resolveDirWorkflowDir dc
                writeYamlFile (wd </> rentTemplatePath) (rentScheduleTemplate :: ScheduleTemplate)
                writeYamlFile (wd </> haircutTemplatePath) (haircutScheduleTemplate :: ScheduleTemplate)
                sf <- resolveFile td "state-file"
                let sets =
                      Settings
                        { setDirectorySettings = dc,
                          setStateFile = sf,
                          setSchedule =
                            Schedule
                              [ ScheduleItem
                                  { scheduleItemDescription = Just "Pay rent",
                                    scheduleItemRecurrenceType = RentRecurrence,
                                    scheduleItemTemplate = rentTemplatePath,
                                    scheduleItemDestination = DestinationPathTemplate rentDestinationPath,
                                    scheduleItemCronSchedule = everyMinute -- Should definitely get activated
                                  },
                                ScheduleItem
                                  { scheduleItemDescription = Just "Get Haircut",
                                    scheduleItemRecurrenceType = HaircutRecurrence,
                                    scheduleItemTemplate = haircutTemplatePath,
                                    scheduleItemDestination = DestinationPathTemplate haircutDestinationPath,
                                    scheduleItemCronSchedule = everyMinute -- Should definitely get activated
                                  }
                              ],
                          setColourSettings = defaultColourSettings
                        }
                check sets -- The first check
                schedule sets -- The first run
                doesFileExist sf `shouldReturn` True -- There should be a state file now
                doesFileExist (wd </> rentDestinationPath) `shouldReturn` True -- There should be a destination file
                doesFileExist (wd </> haircutDestinationPath) `shouldReturn` True -- There should be a destination file
                check sets -- The second check
