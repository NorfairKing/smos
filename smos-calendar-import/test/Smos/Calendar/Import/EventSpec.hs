{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.EventSpec
  ( spec,
  )
where

import Smos.Calendar.Import.Event
import Smos.Calendar.Import.Event.Gen
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @Event
