{-# LANGUAGE TypeApplications #-}

module Smos.Scheduler.OptParseSpec (spec) where

import OptEnvConf.Test
import Smos.Scheduler.OptParse
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Instructions
  goldenSettingsReferenceDocumentationSpec @Instructions "documentation.txt" "smos-scheduler"
  goldenSettingsNixOptionsSpec @Instructions "options.nix"
