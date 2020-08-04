{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.TimeZoneSpec
  ( spec,
  )
where

import Smos.Calendar.Import.TimeZone
import Smos.Calendar.Import.TimeZone.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @TimeZoneId
  jsonSpecOnValid @TimeZoneId
  genValidSpec @TimeZoneHistory
  jsonSpecOnValid @TimeZoneHistory
  genValidSpec @TimeZoneHistoryRule
  jsonSpecOnValid @TimeZoneHistoryRule
  genValidSpec @UTCOffset
  jsonSpecOnValid @UTCOffset
