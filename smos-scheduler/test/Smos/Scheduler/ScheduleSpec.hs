{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Scheduler.ScheduleSpec (spec) where

import qualified Data.Map as M
import Data.Time
import Data.Time.Zones
import Path
import Smos.CLI.Colour
import Smos.Directory.Resolution
import Smos.Directory.TestUtils
import Smos.Report.Time
import Smos.Scheduler.Commands
import Smos.Scheduler.History
import Smos.Scheduler.OptParse
import Smos.Scheduler.Recurrence
import Smos.Scheduler.Render.Gen ()
import Smos.Scheduler.Schedule
import Smos.Scheduler.Schedule.Gen ()
import System.Cron.Types
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity hiding (check)
import Test.Syd.Validity.Aeson

spec :: Spec
spec = modifyMaxSuccess (`div` 10) $ do
  genValidSpec @UTCTimeTemplate
  jsonSpec @UTCTimeTemplate
  genValidSpec @TimestampTemplate
  jsonSpec @TimestampTemplate
  genValidSpec @EntryTemplate
  jsonSpec @EntryTemplate
  genValidSpec @ScheduleTemplate
  jsonSpec @ScheduleTemplate
  genValidSpec @ScheduleItem
  it "updates the last update to the most recent run after scheduling" $
    forAllValid $ \sn ->
      forAllValid $ \templatePath ->
        -- forAllValid $ \scheduleTemplate ->
        withInterestingStore $ \dc -> do
          let scheduleTemplate = ScheduleTemplate []
          wd <- resolveDirWorkflowDir dc
          writeScheduleTemplate (wd </> templatePath) scheduleTemplate
          let item =
                ScheduleItem
                  { scheduleItemDescription = Just "Rent example",
                    scheduleItemTemplate = fromRelFile templatePath,
                    scheduleItemDestination = DestinationPathTemplate [relfile|projects/rent-[ %F ].smos|],
                    scheduleItemRecurrence = RentRecurrence daily
                  }

          today <- utctDay <$> getCurrentTime
          let yesterday = addDays (-1) today
          let yesterdayNight = LocalTime yesterday midnight

          historyBefore <- readReccurrenceHistory dc utcTZ
          context "yesterday schedule" $
            handleScheduleItem dc utcTZ historyBefore (UTCTime yesterday 0) sn item `shouldReturn` Just yesterdayNight
          historyAfter <- readReccurrenceHistory dc utcTZ

          context "yesterday history" $ case M.lookup sn historyAfter of
            Nothing -> expectationFailure "Should have found the result in the recurrence history"
            Just lt -> lt `shouldBe` LatestActivation yesterdayNight Nothing

          let tonight = LocalTime today midnight
          handleScheduleItem dc utcTZ historyAfter (UTCTime today 0) sn item `shouldReturn` Just tonight
          case M.lookup sn historyAfter of
            Nothing -> expectationFailure "Should have found the result in the recurrence history"
            Just lt -> lt `shouldBe` LatestActivation tonight Nothing
