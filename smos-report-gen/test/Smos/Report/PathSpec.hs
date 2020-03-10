{-# LANGUAGE TypeApplications #-}

module Smos.Report.PathSpec where

import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

import Smos.Report.Path
import Smos.Report.Path.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @RootedPath
  ordSpecOnValid @RootedPath
  genValidSpec @RootedPath
  jsonSpecOnValid @RootedPath
