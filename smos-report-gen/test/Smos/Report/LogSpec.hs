module Smos.Report.LogSpec (spec) where

import Smos.Data
import Smos.Data.Gen ()
import Smos.Report.Log
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "makeLogbookLogEntries" $ it "produces valid events" $ producesValidsOnValids makeLogbookLogEntries
  describe "makeStateHistoryLogEntries" $ do
    it "produces valid events" $ producesValidsOnValids makeStateHistoryLogEntries
    it "produces a log event even if there is only one timestamp" $
      forAllValid $
        \e ->
          forAllValid $ \tse ->
            let entry = e {entryStateHistory = StateHistory [tse]}
             in case makeStateHistoryLogEntries entry of
                  [] -> expectationFailure "Expected at least one log event, got none"
                  [le] -> logEventType le `shouldBe` StateChange Nothing (stateHistoryEntryNewState tse)
                  _ -> expectationFailure "Expected at most one log event, got more than one"
    it "produces a log event per state history entry" $
      forAllValid $
        \e -> length (makeStateHistoryLogEntries e) `shouldBe` length (unStateHistory (entryStateHistory e))
  describe "makeTimestampLogEntries" $ it "produces valid events" $ producesValidsOnValids2 makeTimestampLogEntries
