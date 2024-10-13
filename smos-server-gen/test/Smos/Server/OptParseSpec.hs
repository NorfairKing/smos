{-# LANGUAGE TypeApplications #-}

module Smos.Server.OptParseSpec (spec) where

import OptEnvConf.Test
import Smos.Server.OptParse
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Settings
  goldenSettingsReferenceDocumentationSpec @Settings "documentation.txt" "smos-server"
  goldenSettingsNixOptionsSpec @Settings "options.nix"
