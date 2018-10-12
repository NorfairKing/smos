{-# LANGUAGE TypeApplications #-}

module Smos.Report.QuerySpec where

import Test.Hspec
import Test.Validity

import Smos.Report.Query.Gen()
import Smos.Report.Query

spec :: Spec
spec = do
    eqSpec @Filter
    genValidSpec @Filter
