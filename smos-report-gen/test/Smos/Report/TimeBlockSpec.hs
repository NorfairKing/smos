{-# LANGUAGE TypeApplications #-}

module Smos.Report.TimeBlockSpec where

import Smos.Report.TimeBlock
import Smos.Report.TimeBlock.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @(Block Int Int)
  jsonSpec @(Block Int Int)
  genValidSpec @TimeBlock
  jsonSpec @TimeBlock
