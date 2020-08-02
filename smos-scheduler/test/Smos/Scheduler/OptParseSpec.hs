{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Scheduler.OptParseSpec
  ( spec,
  )
where

import Smos.Scheduler.OptParse.Types
import Smos.Scheduler.Render.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @ScheduleTemplate
  jsonSpecOnValid @ScheduleTemplate
  genValidSpec @EntryTemplate
  jsonSpecOnValid @EntryTemplate
  genValidSpec @TimestampTemplate
  jsonSpecOnValid @TimestampTemplate
  genValidSpec @UTCTimeTemplate
  jsonSpecOnValid @UTCTimeTemplate
