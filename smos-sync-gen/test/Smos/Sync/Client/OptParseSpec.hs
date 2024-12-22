{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.OptParseSpec (spec) where

import OptEnvConf.Test
import Smos.Sync.Client.OptParse
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Instructions
  goldenSettingsReferenceDocumentationSpec @Instructions "documentation.txt" "smos-sync"
  goldenSettingsNixOptionsSpec @Instructions "options.nix"
