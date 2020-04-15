{-# LANGUAGE TypeApplications #-}

module Smos.Report.OptParseSpec where

import Smos.Report.OptParse.Gen ()
import Smos.Report.OptParse.Types
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  eqSpecOnValid @Configuration
  genValidSpec @Configuration
  jsonSpecOnValid @Configuration
