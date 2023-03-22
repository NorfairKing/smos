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
  genValidSpec @KeyConfigs
  genValidSpec @FileKeyConfigs
  genValidSpec @BrowserKeyConfigs
  genValidSpec @HelpKeyConfigs
  genValidSpec @ReportsKeyConfigs
  genValidSpec @NextActionReportKeyConfigs
  genValidSpec @WaitingReportKeyConfigs
  genValidSpec @TimestampsReportKeyConfigs
  genValidSpec @KeybindingsConfiguration
  genValidSpec @Configuration
  describe "FromJSON Configuration" $
    it "parses the default configuration back into itself" $ do
      let config = backToConfiguration defaultConfig
      fromJSON (toJSON config) `shouldBe` JSON.Success config
