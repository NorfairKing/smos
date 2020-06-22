{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.NextSpec where

import Smos.Cursor.Report.Next
import Smos.Cursor.Report.Next.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Optics

spec :: Spec
spec = do
  genValidSpec @NextActionReportCursor
  genValidSpec @NextActionReportCursorSelection
  genValidSpec @NextActionEntryCursor
  describe "nextActionReportCursorNextActionEntryCursorsL" $ lensSpecOnValid nextActionReportCursorNextActionEntryCursorsL
  describe "nextActionReportCursorSelectedNextActionEntryCursorsL" $ lensSpecOnValid nextActionReportCursorSelectedNextActionEntryCursorsL
  describe "nextActionReportCursorSelectionL" $ lensSpecOnValid nextActionReportCursorSelectionL
  describe "nextActionReportCursorFilterBarL" $ lensSpecOnValid nextActionReportCursorFilterBarL
