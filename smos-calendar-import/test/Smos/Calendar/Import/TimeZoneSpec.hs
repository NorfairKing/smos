{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.TimeZoneSpec
  ( spec,
  )
where

import Smos.Calendar.Import.TimeZone
import Smos.Calendar.Import.TimeZone.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @TimeZoneHistory
  jsonSpecOnValid @TimeZoneHistory
  genValidSpec @TimeZoneHistoryRule
  jsonSpecOnValid @TimeZoneHistoryRule
  genValidSpec @UTCOffset
  jsonSpecOnValid @UTCOffset
