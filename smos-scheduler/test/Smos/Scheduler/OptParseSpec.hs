{-# LANGUAGE TypeApplications #-}

module Smos.Scheduler.OptParseSpec
  ( spec,
  )
where

import Smos.Scheduler.OptParse.Types
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
