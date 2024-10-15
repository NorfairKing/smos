{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Scheduler.OptParseSpec (spec) where

import Path
import Smos.Report.Time
import Smos.Scheduler.OptParse
import Smos.Scheduler.Render.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @UTCTimeTemplate
  jsonSpec @UTCTimeTemplate
  genValidSpec @TimestampTemplate
  jsonSpec @TimestampTemplate
  genValidSpec @EntryTemplate
  jsonSpec @EntryTemplate
  genValidSpec @ScheduleTemplate
  jsonSpec @ScheduleTemplate
  genValidSpec @ScheduleItem
  describe "hashScheduleItem" $ do
    it "produces valid hashes" $
      producesValid hashScheduleItem
    it "produces the exact same hash, consistently" $
      renderScheduleItemHash
        ( hashScheduleItem
            ScheduleItem
              { scheduleItemDescription = Just "Foobar",
                scheduleItemTemplate = "template.smos.template",
                scheduleItemDestination = DestinationPathTemplate [relfile|result.smos.template|],
                scheduleItemRecurrence = HaircutRecurrence $ Months 1
              }
        )
        `shouldBe` "sARhcXIVaaVp94P3nKt4HkR8nkM6HgxrwpY5kb3Lvf4="
