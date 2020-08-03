{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.RecurringEventSpec
  ( spec,
  )
where

import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.RecurringEvent.Gen
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @RecurringEvent
  jsonSpecOnValid @RecurringEvent
