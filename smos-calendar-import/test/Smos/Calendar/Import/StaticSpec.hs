{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.StaticSpec
  ( spec,
  )
where

import Smos.Calendar.Import.Static
import Smos.Calendar.Import.Static.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Static
  jsonSpec @Static
