{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.EventSpec
  ( spec,
  )
where

import Smos.Calendar.Import.Event
import Smos.Calendar.Import.Event.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Event
  jsonSpecOnValid @Event
  genValidSpec @Events
  jsonSpecOnValid @Events
