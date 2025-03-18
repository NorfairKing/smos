{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Smos.Scheduler.IntegrationSpec (spec) where

import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Text as T
import Path
import Path.IO
import Smos.Archive.Commands.File
import qualified Smos.Archive.OptParse as Archive
import Smos.CLI.Colour
import Smos.Directory.Resolution
import Smos.Directory.TestUtils
import Smos.Report.Time
import Smos.Scheduler.Commands
import Smos.Scheduler.OptParse
import Smos.Scheduler.Render.Gen ()
import Smos.Scheduler.Schedule
import System.Cron.Types
import Test.Syd

spec :: Spec
spec =
  modifyMaxSuccess (`div` 10) $ do
    it "'just works'" $
      withInterestingStore $ \dc -> do
        let templatePathRent = [relfile|rent.smos.template|]
        let templatePathHaircut = [relfile|rent.smos.template|]
        let destinationPathRent = [relfile|rent.smos|]
        let destinationPathHaircut = [relfile|haircut.smos|]

        wd <- resolveDirWorkflowDir dc
        writeScheduleTemplate (wd </> templatePathRent) $ ScheduleTemplate []
        writeScheduleTemplate (wd </> templatePathHaircut) $ ScheduleTemplate []

        let sets =
              Settings
                { setDirectorySettings = dc,
                  setLogLevel = LevelError,
                  setSchedule =
                    Schedule $
                      M.fromList
                        [ ( "rent",
                            ScheduleItem
                              { scheduleItemDescription = Just "Rent example",
                                scheduleItemTemplateFile = T.pack $ fromRelFile templatePathRent,
                                scheduleItemDestination = DestinationPathTemplate destinationPathRent,
                                scheduleItemRecurrence = RentRecurrence daily -- Should definitely get activated
                              }
                          ),
                          ( "haircut",
                            ScheduleItem
                              { scheduleItemDescription = Just "Haircut example",
                                scheduleItemTemplateFile = T.pack $ fromRelFile templatePathHaircut,
                                scheduleItemDestination = DestinationPathTemplate destinationPathHaircut,
                                scheduleItemRecurrence = HaircutRecurrence $ Days 1
                              }
                          )
                        ],
                  setColourSettings = defaultColourSettings
                }

        let destinationRent = wd </> destinationPathRent
        let destinationHaircut = wd </> destinationPathHaircut

        -- First run
        context "check 1" $ check sets -- The first check, fine
        context "next 1" $ next sets -- The first schedule, should schedule both
        context "schedule 1" $ schedule sets -- The first schedule, should schedule both

        -- Both are scheduled
        context "destination file 1 exists" $ doesFileExist destinationRent `shouldReturn` True
        context "destination file 2 exists" $ doesFileExist destinationHaircut `shouldReturn` True

        -- Archive both files
        let archiveFile f =
              runStderrLoggingT $
                runReaderT
                  (smosArchiveFile f)
                  ( Archive.Settings
                      { Archive.setDirectorySettings = dc,
                        Archive.setLogLevel = LevelError
                      }
                  )
        context "archive rent" $ archiveFile destinationRent
        context "archive haircut" $ archiveFile destinationHaircut

        -- Both have been deleted (archived)
        context "destination file 1 exists" $ doesFileExist destinationRent `shouldReturn` False
        context "destination file 2 exists" $ doesFileExist destinationHaircut `shouldReturn` False

        -- Second run
        context "check 2" $ check sets -- The second check, still fine
        context "next 2" $ next sets -- The second check, still fine
        context "schedule 2" $ schedule sets -- The second schedule, should not schedule anything

        -- Neither is activated
        context "destination file 1 exists" $ doesFileExist destinationRent `shouldReturn` False
        context "destination file 2 exists" $ doesFileExist destinationHaircut `shouldReturn` False
