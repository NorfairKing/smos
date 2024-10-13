{-# LANGUAGE TypeApplications #-}

module Smos.Query.OptParseSpec (spec) where

import OptEnvConf.Test
import Smos.Query.OptParse
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Instructions
  goldenSettingsReferenceDocumentationSpec @Instructions "documentation.txt" "smos-query"
  goldenSettingsNixOptionsSpec @Instructions "options.nix"
