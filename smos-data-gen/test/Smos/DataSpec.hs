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
      producesValidsOnValids2 smosFileClockOutEverywhere
  describe "entryClockIn" $ it "produces valid entries" $ producesValidsOnValids2 entryClockIn
  describe "entryClockOut" $ it "produces valid entries" $ producesValidsOnValids2 entryClockOut
  describe "logbookClockIn" $ it "produces valid logbooks" $ producesValidsOnValids2 logbookClockIn
  describe "logbookClockOut" $
    it "produces valid logbooks" $
      producesValidsOnValids2 logbookClockOut
