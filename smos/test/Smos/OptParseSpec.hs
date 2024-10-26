{-# LANGUAGE TypeApplications #-}

module Smos.OptParseSpec (spec) where

import OptEnvConf.Test
import Smos.OptParse
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Instructions
  goldenSettingsReferenceDocumentationSpec @Instructions "documentation.txt" "smos"
  goldenSettingsNixOptionsSpec @Instructions "options.nix"
