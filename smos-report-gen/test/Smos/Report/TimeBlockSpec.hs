{-# LANGUAGE TypeApplications #-}

module Smos.Report.TimeBlockSpec where

import Smos.Report.TimeBlock
import Smos.Report.TimeBlock.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @(Block Int Int)
  jsonSpecOnValid @(Block Int Int)
  genValidSpec @TimeBlock
  jsonSpecOnValid @TimeBlock
