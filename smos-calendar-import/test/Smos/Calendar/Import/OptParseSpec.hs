{-# LANGUAGE TypeApplications #-}

module Smos.Calendar.Import.OptParseSpec (spec) where

import OptEnvConf.Test
import Smos.Calendar.Import.OptParse
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Settings
  goldenSettingsReferenceDocumentationSpec @Settings "documentation.txt" "smos-calendar-import"
  goldenSettingsNixOptionsSpec @Settings "options.nix"
