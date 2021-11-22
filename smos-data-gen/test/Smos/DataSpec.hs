module Smos.DataSpec
  ( spec,
  )
where

import Smos.Data
import Smos.Data.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "smosFileClockOutEverywhere" $
    it "produces valid smos files" $
      producesValid2 smosFileClockOutEverywhere
  describe "entryClockIn" $ it "produces valid entries" $ producesValid2 entryClockIn
  describe "entryClockOut" $ it "produces valid entries" $ producesValid2 entryClockOut
  describe "logbookClockIn" $ it "produces valid logbooks" $ producesValid2 logbookClockIn
  describe "logbookClockOut" $
    it "produces valid logbooks" $
      producesValid2 logbookClockOut
