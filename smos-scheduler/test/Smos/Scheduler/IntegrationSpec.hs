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
import Smos.Report.Time
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
    forAllValid $ \templatePath ->
      forAll (genValid `suchThat` (/= templatePath)) $ \destinationPath1 ->
        forAll (genValid `suchThat` (\p -> p /= templatePath && p /= destinationPath1)) $ \destinationPath2 ->
          withInterestingStore $ \dc -> do
            let scheduleTemplate = ScheduleTemplate []
            wd <- resolveDirWorkflowDir dc
            writeYamlFile (wd </> templatePath) (scheduleTemplate :: ScheduleTemplate)
            let sets =
                  Settings
                    { setDirectorySettings = dc,
                      setSchedule =
                        Schedule
                          [ ScheduleItem
                              { scheduleItemDescription = Just "Rent example",
                                scheduleItemTemplate = templatePath,
                                scheduleItemDestination = DestinationPathTemplate destinationPath1,
                                scheduleItemRecurrence = RentRecurrence everyMinute -- Should definitely get activated
                              },
                            ScheduleItem
                              { scheduleItemDescription = Just "Haircut example",
                                scheduleItemTemplate = templatePath,
                                scheduleItemDestination = DestinationPathTemplate destinationPath2,
                                scheduleItemRecurrence = HaircutRecurrence $ Minutes 1
                              }
                          ],
                      setColourSettings = defaultColourSettings
                    }
            check sets -- The first check
            schedule sets -- The first run
            context "destination file 1 exists" $ doesFileExist (wd </> destinationPath1) `shouldReturn` True
            context "destination file 2 exists" $ doesFileExist (wd </> destinationPath2) `shouldReturn` True
            check sets -- The second check
