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
  jsonSpecOnValid @KeyConfig
  genValidSpec @KeyConfigs
  jsonSpecOnValid @KeyConfigs
  genValidSpec @FileKeyConfigs
  jsonSpecOnValid @FileKeyConfigs
  genValidSpec @BrowserKeyConfigs
  jsonSpecOnValid @BrowserKeyConfigs
  genValidSpec @HelpKeyConfigs
  jsonSpecOnValid @HelpKeyConfigs
  genValidSpec @ReportsKeyConfigs
  jsonSpecOnValid @ReportsKeyConfigs
  genValidSpec @NextActionReportKeyConfigs
  jsonSpecOnValid @NextActionReportKeyConfigs
  genValidSpec @WaitingReportKeyConfigs
  jsonSpecOnValid @WaitingReportKeyConfigs
  genValidSpec @KeybindingsConfiguration
  jsonSpecOnValid @KeybindingsConfiguration
  genValidSpec @Configuration
  jsonSpecOnValid @Configuration
  describe "FromJSON Configuration"
    $ it "parses the default configuration back into itself"
    $ do
      let config = backToConfiguration defaultConfig
      fromJSON (toJSON config) `shouldBe` JSON.Success config
