{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.WaitingSpec where

import Smos.Cursor.Report.Waiting
import Smos.Cursor.Report.Waiting.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Optics

spec :: Spec
spec = do
  genValidSpec @WaitingReportCursor
  genValidSpec @WaitingEntryCursor
  describe "waitingReportCursorNonEmptyCursorL" $ lensSpecOnValid waitingReportCursorNonEmptyCursorL
