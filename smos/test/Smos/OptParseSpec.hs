{-# LANGUAGE TypeApplications #-}

module Smos.OptParseSpec where

import TestImport

import Data.Aeson as JSON

import Smos.Default
import Smos.OptParse.Gen ()
import Smos.OptParse.Types

spec :: Spec
spec = do
  eqSpecOnValid @KeyConfig
  genValidSpec @KeyConfig
  jsonSpecOnValid @KeyConfig
  eqSpecOnValid @KeyConfigs
  genValidSpec @KeyConfigs
  jsonSpecOnValid @KeyConfigs
  eqSpecOnValid @FileKeyConfigs
  genValidSpec @FileKeyConfigs
  jsonSpecOnValid @FileKeyConfigs
  eqSpecOnValid @HelpKeyConfigs
  genValidSpec @HelpKeyConfigs
  jsonSpecOnValid @HelpKeyConfigs
  eqSpecOnValid @ReportsKeyConfigs
  genValidSpec @ReportsKeyConfigs
  jsonSpecOnValid @ReportsKeyConfigs
  eqSpecOnValid @KeybindingsConfiguration
  genValidSpec @KeybindingsConfiguration
  jsonSpecOnValid @KeybindingsConfiguration
  eqSpecOnValid @Configuration
  genValidSpec @Configuration
  jsonSpecOnValid @Configuration
  describe "FromJSON Configuration" $
    it "parses the default configuration back into itself" $ do
      let config = backToConfiguration defaultConfig
      fromJSON (toJSON config) `shouldBe` JSON.Success config
