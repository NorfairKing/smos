{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.WaitingSpec where

import Smos.Cursor.Report.Waiting
import Smos.Cursor.Report.Waiting.Gen ()
import Smos.Report.TestUtils
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity
import Test.Validity.Optics

spec :: Spec
spec = do
  genValidSpec @WaitingReportCursor
  genValidSpec @WaitingEntryCursor
  describe "makeWaitingEntryCursor" $ it "produces valid cursors" $ producesValidsOnValids2 makeWaitingEntryCursor
  modifyMaxSuccess (`div` 10) $ describe "produceWaitingReportCursor" $ it "produces valid reports for interesting stores"
    $ withInterestingStore
    $ \dc -> do
      wrc <- produceWaitingReportCursor dc
      shouldBeValid wrc
