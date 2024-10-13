{-# LANGUAGE TypeApplications #-}

module Smos.Notify.OptParseSpec (spec) where

import OptEnvConf.Test
import Smos.Notify.OptParse
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Settings
  goldenSettingsReferenceDocumentationSpec @Settings "documentation.txt" "smos-notify"
  goldenSettingsNixOptionsSpec @Settings "options.nix"
