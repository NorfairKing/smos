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
import Data.Maybe
import Smos.Report.Comparison
import Smos.Report.Comparison.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Comparison
  jsonSpecOnValid @Comparison
  describe "parseComparison"
    $ it "produces valid comparisons"
    $ producesValidsOnValids parseComparison
  describe "renderComparison" $ do
    it "produces valid comparisons" $ producesValidsOnValids renderComparison
    it "is the inverse of parseComparison" $
      inverseFunctionsOnValid renderComparison (fromJust . parseComparison)
