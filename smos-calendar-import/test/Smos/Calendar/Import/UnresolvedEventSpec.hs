{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.UnresolvedEventSpec
  ( spec,
  )
where

import Smos.Calendar.Import.UnresolvedEvent
import Smos.Calendar.Import.UnresolvedEvent.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @UnresolvedEvent
  jsonSpecOnValid @UnresolvedEvent
  genValidSpec @UnresolvedEventGroup
  jsonSpecOnValid @UnresolvedEventGroup
  genValidSpec @UnresolvedEvents
  jsonSpecOnValid @UnresolvedEvents
