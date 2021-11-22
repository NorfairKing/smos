{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.RecurringEventSpec
  ( spec,
  )
where

import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.RecurringEvent.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @RecurringEvents
  jsonSpec @RecurringEvents
  genValidSpec @RecurringEvent
  jsonSpec @RecurringEvent
  genValidSpec @Recurrence
  jsonSpec @Recurrence
