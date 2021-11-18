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
  jsonSpecOnValid @UTCTimeTemplate
  genValidSpec @TimestampTemplate
  jsonSpecOnValid @TimestampTemplate
  genValidSpec @EntryTemplate
  jsonSpecOnValid @EntryTemplate
  genValidSpec @ScheduleTemplate
  jsonSpecOnValid @ScheduleTemplate
