{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Smos.Scheduler.ScheduleSpec (spec) where

import qualified Data.Map as M
import Data.Time
import Path
import Smos.CLI.Colour
import Smos.Directory.Resolution
import Smos.Directory.TestUtils
import Smos.Report.Time
import Smos.Scheduler.Commands
import Smos.Scheduler.OptParse
import Smos.Scheduler.Recurrence
import Smos.Scheduler.Render.Gen ()
import System.Cron.Types
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity hiding (check)

spec :: Spec
spec = modifyMaxSuccess (`div` 10) $ do
  it "upates the last update to the most recent run after scheduling" $
    forAllValid $ \templatePath ->
      forAllValid $ \scheduleTemplate ->
        withInterestingStore $ \dc -> do
          wd <- resolveDirWorkflowDir dc
          writeScheduleTemplate (wd </> templatePath) scheduleTemplate
          let item =
                ScheduleItem
                  { scheduleItemDescription = Just "Rent example",
                    scheduleItemTemplate = fromRelFile templatePath,
                    scheduleItemDestination = DestinationPathTemplate [relfile|projects/rent-[ %F ].smos|],
                    scheduleItemRecurrence = RentRecurrence daily
                  }
          let h = hashScheduleItem item

          today <- utctDay <$> getCurrentTime
          let yesterday = addDays (-1) today
          let yesterdayNight = LocalTime yesterday midnight

          historyBefore <- readReccurrenceHistory dc
          print historyBefore
          r <- performScheduleItem dc yesterdayNight item
          print r
          historyAfter <- readReccurrenceHistory dc
          print historyAfter

          case M.lookup h historyAfter of
            Nothing -> expectationFailure "Should have found the result in the recurrence history"
            Just _ -> undefined
