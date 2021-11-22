{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.UnresolvedTimestampSpec
  ( spec,
  )
where

import Smos.Calendar.Import.UnresolvedTimestamp
import Smos.Calendar.Import.UnresolvedTimestamp.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @CalRDate
  jsonSpec @CalRDate
  genValidSpec @CalPeriod
  jsonSpec @CalPeriod
  genValidSpec @CalEndDuration
  jsonSpec @CalEndDuration
  genValidSpec @CalTimestamp
  jsonSpec @CalTimestamp
  genValidSpec @CalDateTime
  jsonSpec @CalDateTime
  genValidSpec @TimeZoneId
  jsonSpec @TimeZoneId
