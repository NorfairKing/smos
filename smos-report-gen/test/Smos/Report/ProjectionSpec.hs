{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.ProjectionSpec where

import Data.Text (Text)

import Test.Hspec
import Test.Validity

import Text.Megaparsec

import Cursor.Forest.Gen ()

import Smos.Report.Path.Gen ()
import Smos.Report.Projection
import Smos.Report.Projection.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @Projection
  genValidSpec @Projection
  describe "performProjection" $
    it "produces valid projections" $
    forAllValid $ \s ->
      forAllValid $ \rp ->
        forAllValid $ \fc -> shouldBeValid $ performProjection s rp fc
  describe "ontoFileP" $ parsesValidSpec ontoFileP
  describe "ontoPropertyP" $ parsesValidSpec ontoPropertyP
  describe "ontoHeaderP" $ parsesValidSpec ontoHeaderP
  describe "ontoStateP" $ parsesValidSpec ontoStateP
  describe "andAlsoP" $ parsesValidSpec andAlsoP
  describe "projectionP" $ do
    parsesValidSpec projectionP
    parseJustSpec projectionP "file" OntoFile
    parseJustSpec projectionP "property:effort" $ OntoProperty "effort"
    parseJustSpec projectionP "header" OntoHeader
    parseJustSpec projectionP "state" OntoState
    parseJustSpec projectionP "(property:effort also file)" $
      AndAlso (OntoProperty "effort") OntoFile
  describe "renderProjection" $ do
    it "produces valid texts" $ producesValidsOnValids renderProjection
    it "renders bys that parse to the same" $
      forAllValid $ \s -> parseJust projectionP (renderProjection s) s

parseJustSpec :: (Show a, Eq a) => P a -> Text -> a -> Spec
parseJustSpec p s res =
  it (unwords ["parses", show s, "as", show res]) $ parseJust p s res

parseNothingSpec :: (Show a, Eq a) => P a -> Text -> Spec
parseNothingSpec p s =
  it (unwords ["fails to parse", show s]) $ parseNothing p s

parsesValidSpec :: (Show a, Eq a, Validity a) => P a -> Spec
parsesValidSpec p = it "only parses valid values" $ forAllValid $ parsesValid p

parseJust :: (Show a, Eq a) => P a -> Text -> a -> Expectation
parseJust p s res =
  case parse (p <* eof) "test input" s of
    Left err ->
      expectationFailure $
      unlines ["P failed on input", show s, "with error", parseErrorPretty err]
    Right out -> out `shouldBe` res

parseNothing :: (Show a, Eq a) => P a -> Text -> Expectation
parseNothing p s =
  case parse (p <* eof) "test input" s of
    Right v ->
      expectationFailure $
      unlines
        [ "P succeeded on input"
        , show s
        , "at parsing"
        , show v
        , "but it should have failed."
        ]
    Left _ -> pure ()

parsesValid :: (Show a, Eq a, Validity a) => P a -> Text -> Expectation
parsesValid p s =
  case parse (p <* eof) "test input" s of
    Left _ -> pure ()
    Right out -> shouldBeValid out
