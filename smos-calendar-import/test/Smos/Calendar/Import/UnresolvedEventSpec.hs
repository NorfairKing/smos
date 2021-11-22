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
  jsonSpec @UnresolvedEvent
  genValidSpec @UnresolvedEventGroup
  jsonSpec @UnresolvedEventGroup
  genValidSpec @UnresolvedEvents
  jsonSpec @UnresolvedEvents
