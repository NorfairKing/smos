{-# LANGUAGE TypeApplications #-}

module Smos.Web.Server.OptParseSpec (spec) where

import OptEnvConf.Test
import Smos.Web.Server.OptParse
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Settings
  goldenSettingsReferenceDocumentationSpec @Settings "documentation.txt" "smos-web-server"
  goldenSettingsNixOptionsSpec @Settings "options.nix"
