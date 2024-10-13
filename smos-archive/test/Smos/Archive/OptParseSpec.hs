{-# LANGUAGE TypeApplications #-}

module Smos.Archive.OptParseSpec (spec) where

import OptEnvConf.Test
import Smos.Archive.OptParse
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Instructions
  goldenSettingsReferenceDocumentationSpec @Instructions "documentation.txt" "smos-archive"
  goldenSettingsNixOptionsSpec @Instructions "options.nix"
