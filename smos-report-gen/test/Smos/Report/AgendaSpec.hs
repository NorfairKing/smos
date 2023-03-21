{-# LANGUAGE TypeApplications #-}

module Smos.Report.AgendaSpec where

import Smos.Directory.TestUtils
import Smos.Report.Agenda
import Smos.Report.Agenda.Gen ()
import Smos.Report.Archive.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.ShouldPrint
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @AgendaEntry
  jsonSpec @AgendaEntry
  genValidSpec @AgendaTodayReport
  jsonSpec @AgendaTodayReport
  genValidSpec @AgendaReport
  jsonSpec @AgendaReport
  describe "makeAgendaReport" $ do
    it "produces valid reports" $
      forAllValid $ \zt ->
        forAllValid $ \p ->
          forAllValid $ \tb ->
            forAllValid $ \aes ->
              shouldBeValid $ makeAgendaReport zt p tb aes
  modifyMaxSuccess (`div` 10) $
    describe "produceAgendaReport" $
      it "produces valid reports" $
        forAllValid $ \zt ->
          forAllValid $ \p ->
            forAllValid $ \tb ->
              forAllValid $ \ha ->
                forAllValid $ \ah ->
                  forAllValid $ \mFilter ->
                    withInterestingStore $ \dc -> do
                      ar <- produceAgendaReport zt p tb ha DontPrint ah mFilter dc
                      shouldBeValid ar
