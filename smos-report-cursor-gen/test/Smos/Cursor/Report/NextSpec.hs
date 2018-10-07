{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.NextSpec where

import Test.Hspec
import Test.Validity

import Smos.Cursor.Report.Next

import Smos.Cursor.Report.Next.Gen ()

spec :: Spec
spec = do
    eqSpec @NextActionReportCursor
    genValidSpec @NextActionReportCursor
    eqSpec @NextActionEntryCursor
    genValidSpec @NextActionEntryCursor
