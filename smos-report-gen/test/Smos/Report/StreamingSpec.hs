{-# LANGUAGE TypeApplications #-}

module Smos.Report.StreamingSpec where

import Test.Hspec
import Test.Validity

import Smos.Data

import Data.GenValidity.Containers ()

import Smos.Data.Gen ()

import Smos.Report.Streaming

spec :: Spec
spec =
    describe "forestCursors" $ do
    it "produces valid forests" $
     producesValidsOnValids
        (forestCursors @Entry)
    it
        "produces congruent forests" $
     forAllValid $ \f -> () <$ forestCursors @Entry f `shouldBe` () <$ f
