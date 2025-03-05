{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Scheduler.OptParseSpec (spec) where

import OptEnvConf.Test
import Path
import Smos.Report.Time
import Smos.Scheduler.OptParse
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  settingsLintSpec @Instructions
  goldenSettingsReferenceDocumentationSpec @Instructions "documentation.txt" "smos-scheduler"
  goldenSettingsNixOptionsSpec @Instructions "options.nix"
