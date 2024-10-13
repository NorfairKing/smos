{-# LANGUAGE TypeApplications #-}

module Smos.Single.OptParseSpec (spec) where

import OptEnvConf.Test
import Smos.Single.OptParse
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Settings
  goldenSettingsReferenceDocumentationSpec @Settings "documentation.txt" "smos-single"
  goldenSettingsNixOptionsSpec @Settings "options.nix"
