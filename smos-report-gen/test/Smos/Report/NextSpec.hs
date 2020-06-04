{-# LANGUAGE TypeApplications #-}

module Smos.Report.NextSpec where

import Smos.Report.Next
import Smos.Report.Next.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @NextActionReport
  jsonSpecOnValid @NextActionReport
  genValidSpec @NextActionEntry
  jsonSpecOnValid @NextActionEntry
