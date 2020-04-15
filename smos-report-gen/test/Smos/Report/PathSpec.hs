{-# LANGUAGE TypeApplications #-}

module Smos.Report.PathSpec where

import Smos.Report.Path
import Smos.Report.Path.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  eqSpecOnValid @RootedPath
  ordSpecOnValid @RootedPath
  genValidSpec @RootedPath
  jsonSpecOnValid @RootedPath
