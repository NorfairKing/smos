{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.ComparisonSpec
  ( spec
  ) where

import Data.Char as Char
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Path

import Test.Hspec
import Test.QuickCheck as QC
import Test.Validity
import Test.Validity.Aeson

import Control.Monad

import Text.Megaparsec

import Cursor.Forest.Gen ()
import Cursor.Simple.Forest

import Smos.Data

import Smos.Report.Path.Gen ()

import Smos.Report.Comparison
import Smos.Report.Comparison.Gen ()
import Smos.Report.Path
import Smos.Report.Time hiding (P)

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
