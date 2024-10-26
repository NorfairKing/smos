{-# LANGUAGE TypeApplications #-}

module Smos.JobHunt.OptParseSpec (spec) where

import OptEnvConf.Test
import Smos.JobHunt.OptParse
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Instructions
  goldenSettingsReferenceDocumentationSpec @Instructions "documentation.txt" "smos-jobhunt"
  goldenSettingsNixOptionsSpec @Instructions "options.nix"
