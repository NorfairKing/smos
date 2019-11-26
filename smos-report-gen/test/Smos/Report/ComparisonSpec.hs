{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.ComparisonSpec
  ( spec
  ) where

import Data.Maybe

import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

import Cursor.Forest.Gen ()

import Smos.Report.Path.Gen ()

import Smos.Report.Comparison
import Smos.Report.Comparison.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @Comparison
  genValidSpec @Comparison
  jsonSpecOnValid @Comparison
  describe "parseComparison" $
    it "produces valid comparisons" $ producesValidsOnValids parseComparison
  describe "renderComparison" $ do
    it "produces valid comparisons" $ producesValidsOnValids renderComparison
    it "is the inverse of parseComparison" $
      inverseFunctionsOnValid renderComparison (fromJust . parseComparison)
