{-# LANGUAGE TypeApplications #-}

module Smos.Report.AgendaSpec where

import Smos.Report.Agenda
import Smos.Report.Agenda.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @AgendaReport
  jsonSpecOnValid @AgendaReport
  genValidSpec @AgendaTodayReport
  jsonSpecOnValid @AgendaTodayReport
  genValidSpec @AgendaEntry
  jsonSpecOnValid @AgendaEntry
