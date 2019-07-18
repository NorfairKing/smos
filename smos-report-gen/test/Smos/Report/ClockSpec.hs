module Smos.Report.ClockSpec where

import Test.Hspec
import Test.Validity

import Data.GenValidity.Path ()

import Smos.Data.Gen ()

import Smos.Report.Clock

import Smos.Report.Clock.Gen ()
import Smos.Report.Filter.Gen ()

spec :: Spec
spec = do
  describe "zeroOutByFilter" $ do
    it "produces valid smos files" $ producesValidsOnValids3 zeroOutByFilter
  describe "trimLogbookEntry" $
    it "produces valid logbook entries" $
    producesValidsOnValids3 trimLogbookEntry
  describe "trimLogbookEntryTo" $
    it "produces valid logbook entries" $
    forAllValid $ \tz -> producesValidsOnValids3 $ trimLogbookEntryTo tz
  describe "sumLogbookEntryTime" $
    it "produces valid difftimes" $ producesValidsOnValids sumLogbookEntryTime
