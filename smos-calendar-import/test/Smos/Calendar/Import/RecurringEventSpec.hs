{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.RecurringEventSpec
  ( spec,
  )
where

import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.RecurringEvent.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @RecurringEvents
  jsonSpecOnValid @RecurringEvents
  genValidSpec @RecurringEvent
  jsonSpecOnValid @RecurringEvent
  genValidSpec @CalEndDuration
  jsonSpecOnValid @CalEndDuration
  genValidSpec @CalTimestamp
  jsonSpecOnValid @CalTimestamp
  genValidSpec @CalDateTime
  jsonSpecOnValid @CalDateTime
