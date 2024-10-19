{-# LANGUAGE TypeApplications #-}

module Smos.OptParseSpec where

import Smos.OptParse
import Smos.OptParse.Gen ()
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
