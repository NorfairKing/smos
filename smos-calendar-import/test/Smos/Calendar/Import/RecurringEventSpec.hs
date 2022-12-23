{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.RecurringEventSpec
  ( spec,
  )
where

import ICal.Gen ()
import ICal.Recurrence.Gen ()
import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.RecurringEvent.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @RecurringEvents
  jsonSpec @RecurringEvents
