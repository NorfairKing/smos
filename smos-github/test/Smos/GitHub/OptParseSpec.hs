{-# LANGUAGE TypeApplications #-}

module Smos.GitHub.OptParseSpec (spec) where

import OptEnvConf.Test
import Smos.GitHub.OptParse
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Instructions
  goldenSettingsReferenceDocumentationSpec @Instructions "documentation.txt" "smos-github"
  goldenSettingsNixOptionsSpec @Instructions "options.nix"
