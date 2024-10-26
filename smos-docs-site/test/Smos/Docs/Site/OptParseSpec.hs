{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.OptParseSpec (spec) where

import OptEnvConf.Test
import Smos.Docs.Site.OptParse
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Settings
  goldenSettingsReferenceDocumentationSpec @Settings "documentation.txt" "smos-docs-site"
  goldenSettingsNixOptionsSpec @Settings "options.nix"
