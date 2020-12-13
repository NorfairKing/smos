{-# LANGUAGE TypeApplications #-}

module Smos.Report.OptParseSpec where

import Smos.Report.OptParse.Gen ()
import Smos.Report.OptParse.Types
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Configuration
  jsonSpecOnValid @Configuration
  genValidSpec @DirectoryConfiguration
  jsonSpecOnValid @DirectoryConfiguration
  genValidSpec @WorkReportConfiguration
  jsonSpecOnValid @WorkReportConfiguration
