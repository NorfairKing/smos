{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.UnresolvedTimestampSpec
  ( spec,
  )
where

import Smos.Calendar.Import.UnresolvedTimestamp
import Smos.Calendar.Import.UnresolvedTimestamp.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @CalEndDuration
  jsonSpecOnValid @CalEndDuration
  genValidSpec @CalTimestamp
  jsonSpecOnValid @CalTimestamp
  genValidSpec @CalDateTime
  jsonSpecOnValid @CalDateTime
