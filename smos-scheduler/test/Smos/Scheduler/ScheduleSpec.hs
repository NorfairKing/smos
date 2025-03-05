{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Scheduler.ScheduleSpec (spec) where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import Path
import Path.IO
import Smos.Directory.Resolution
import Smos.Directory.TestUtils
import Smos.Scheduler.Commands
import Smos.Scheduler.History
import Smos.Scheduler.Render.Gen ()
import Smos.Scheduler.Schedule
import Smos.Scheduler.Schedule.Gen ()
import System.Cron.Types
import Test.Syd
import Test.Syd.Validity hiding (check)
import Test.Syd.Validity.Aeson

spec :: Spec
spec = modifyMaxSuccess (`div` 10) $ do
  genValidSpec @ScheduleItem
  jsonSpec @ScheduleItem

  describe "readScheduleTemplate" $
    it "roundtrips what writeScheduleTemplate writes" $
      forAllValid $ \scheduleTemplate ->
        withSystemTempDir "smos-scheduler" $ \tdir -> do
          f <- resolveFile tdir "example"
          writeScheduleTemplate f scheduleTemplate
          result <- readScheduleTemplate f
          result `shouldBe` Just (Right scheduleTemplate)

  it "updates the last update to the most recent run after scheduling" $
    forAllValid $ \sn ->
      forAllValid $ \templatePath ->
        withInterestingStore $ \dc -> do
          let scheduleTemplate = ScheduleTemplate []
          wd <- resolveDirWorkflowDir dc
          writeScheduleTemplate (wd </> templatePath) scheduleTemplate
          let item =
                ScheduleItem
                  { scheduleItemDescription = Just "Rent example",
                    scheduleItemTemplateFile = T.pack $ fromRelFile templatePath,
                    scheduleItemDestination = DestinationPathTemplate [relfile|projects/rent-[ %F ].smos|],
                    scheduleItemRecurrence = RentRecurrence daily
                  }

          today <- utctDay <$> getCurrentTime
          let tonight = LocalTime today midnight
          let yesterday = addDays (-1) today
          let yesterdayNight = LocalTime yesterday midnight
          let twoDaysAgo = addDays (-2) today
          let twoDaysAgoNight = LocalTime twoDaysAgo midnight

          historyBefore <- readReccurrenceHistory dc
          context "yesterday schedule" $
            handleScheduleItem dc historyBefore twoDaysAgoNight sn item `shouldReturn` Just yesterdayNight
          historyAfter <- readReccurrenceHistory dc

          context "yesterday history" $ case M.lookup sn historyAfter of
            Nothing -> expectationFailure "Should have found the result in the recurrence history"
            Just lt -> lt `shouldBe` LatestActivation yesterdayNight Nothing

          context "today schedule" $
            handleScheduleItem dc historyAfter tonight sn item `shouldReturn` Just tonight
          historyEnd <- readReccurrenceHistory dc

          context "today history" $
            case M.lookup sn historyEnd of
              Nothing -> expectationFailure "Should have found the result in the recurrence history"
              Just lt -> lt `shouldBe` LatestActivation tonight Nothing
