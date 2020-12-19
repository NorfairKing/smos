module Smos.Report.ClockSpec where

import Data.GenValidity.Path ()
import Smos.Data.Gen ()
import Smos.Report.Clock
import Smos.Report.Clock.Gen ()
import Smos.Report.Filter.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "zeroOutByFilter" $
    it "produces valid smos files" $
      producesValidsOnValids3 zeroOutByFilter
  describe "trimLogbookEntry" $
    it "produces valid logbook entries" $
      producesValidsOnValids3 trimLogbookEntry
  describe "trimLogbookEntryTo" $
    it "produces valid logbook entries" $
      forAllValid $
        \tz -> producesValidsOnValids3 $ trimLogbookEntryTo tz
  describe "sumLogbookEntryTime" $
    it "produces valid difftimes" $
      producesValidsOnValids sumLogbookEntryTime
