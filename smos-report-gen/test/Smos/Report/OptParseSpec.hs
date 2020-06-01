{-# LANGUAGE TypeApplications #-}

module Smos.Report.OptParseSpec where

import Smos.Report.OptParse.Gen ()
import Smos.Report.OptParse.Types
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Configuration
  jsonSpecOnValid @Configuration
  genValidSpec @DirectoryConfiguration
  jsonSpecOnValid @DirectoryConfiguration
  genValidSpec @WorkReportConfiguration
  jsonSpecOnValid @WorkReportConfiguration
