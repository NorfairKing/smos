{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Report.ComparisonSpec
  ( spec,
  )
where

import Cursor.Forest.Gen ()
import Smos.Report.Comparison
import Smos.Report.Comparison.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Comparison
  jsonSpec @Comparison
  describe "parseComparison" $
    it "produces valid comparisons" $
      producesValid parseComparison
  describe "renderComparison" $ do
    it "produces valid comparisons" $ producesValid renderComparison
    it "is the inverse of parseComparison" $
      inverseFunctions renderComparison (either (error "should have worked") id . parseComparison)
