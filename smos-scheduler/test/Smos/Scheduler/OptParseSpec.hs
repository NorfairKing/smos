{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Scheduler.OptParseSpec (spec) where

import OptEnvConf.Test
import Path
import Smos.Report.Time
import Smos.Scheduler.OptParse
import Smos.Scheduler.Render.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  settingsLintSpec @Instructions
  goldenSettingsReferenceDocumentationSpec @Instructions "documentation.txt" "smos-scheduler"
  goldenSettingsNixOptionsSpec @Instructions "options.nix"
  genValidSpec @UTCTimeTemplate
  jsonSpec @UTCTimeTemplate
  genValidSpec @TimestampTemplate
  jsonSpec @TimestampTemplate
  genValidSpec @EntryTemplate
  jsonSpec @EntryTemplate
  genValidSpec @ScheduleTemplate
  jsonSpec @ScheduleTemplate
  genValidSpec @ScheduleItem
