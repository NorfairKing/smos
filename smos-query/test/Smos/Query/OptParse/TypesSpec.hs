{-# LANGUAGE TypeApplications #-}

module Smos.Query.OptParse.TypesSpec (spec) where

import Smos.Query.OptParse.Gen ()
import Smos.Query.OptParse.Types
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  jsonSpec @PreparedReportConfiguration
  genValidSpec @PreparedReportConfiguration
