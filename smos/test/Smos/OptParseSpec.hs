{-# LANGUAGE TypeApplications #-}

module Smos.OptParseSpec where

import Data.Aeson as JSON
import Smos.Default
import Smos.OptParse.Gen ()
import Smos.OptParse.Types
import TestImport

spec :: Spec
spec = do
  genValidSpec @KeyConfig
  jsonSpec @KeyConfig
  genValidSpec @KeyConfigs
  jsonSpec @KeyConfigs
  genValidSpec @FileKeyConfigs
  jsonSpec @FileKeyConfigs
  genValidSpec @BrowserKeyConfigs
  jsonSpec @BrowserKeyConfigs
  genValidSpec @HelpKeyConfigs
  jsonSpec @HelpKeyConfigs
  genValidSpec @ReportsKeyConfigs
  jsonSpec @ReportsKeyConfigs
  genValidSpec @NextActionReportKeyConfigs
  jsonSpec @NextActionReportKeyConfigs
  genValidSpec @WaitingReportKeyConfigs
  jsonSpec @WaitingReportKeyConfigs
  genValidSpec @TimestampsReportKeyConfigs
  jsonSpec @TimestampsReportKeyConfigs
  genValidSpec @KeybindingsConfiguration
  jsonSpec @KeybindingsConfiguration
  genValidSpec @Configuration
  jsonSpec @Configuration
  describe "FromJSON Configuration" $
    it "parses the default configuration back into itself" $
      do
        let config = backToConfiguration defaultConfig
        fromJSON (toJSON config) `shouldBe` JSON.Success config
