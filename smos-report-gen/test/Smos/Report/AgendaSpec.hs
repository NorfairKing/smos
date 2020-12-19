{-# LANGUAGE TypeApplications #-}

module Smos.Report.AgendaSpec where

import Smos.Report.Agenda
import Smos.Report.Agenda.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @AgendaEntry
  jsonSpecOnValid @AgendaEntry
  genValidSpec @AgendaTodayReport
  jsonSpecOnValid @AgendaTodayReport
  genValidSpec @AgendaReport
  jsonSpecOnValid @AgendaReport
  describe "makeAgendaReport" $ do
    it "produces valid reports" $
      forAllValid $
        \zt ->
          forAllValid $ \p ->
            forAllValid $ \tb ->
              forAllValid $ \aes ->
                shouldBeValid $ makeAgendaReport zt p tb aes
