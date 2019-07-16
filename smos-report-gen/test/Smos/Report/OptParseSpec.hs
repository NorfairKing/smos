{-# LANGUAGE TypeApplications #-}

module Smos.Report.OptParseSpec where

import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

import Smos.Report.OptParse.Gen ()
import Smos.Report.OptParse.Types

spec :: Spec
spec = do
  eqSpec @Configuration
  genValidSpec @Configuration
  jsonSpecOnValid @Configuration
