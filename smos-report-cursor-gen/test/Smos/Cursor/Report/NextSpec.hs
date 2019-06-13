{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.NextSpec where

import Test.Hspec
import Test.Validity

import Smos.Cursor.Report.Next

import Smos.Cursor.Report.Next.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @NextActionReportCursor
  genValidSpec @NextActionReportCursor
  eqSpecOnValid @NextActionEntryCursor
  genValidSpec @NextActionEntryCursor
