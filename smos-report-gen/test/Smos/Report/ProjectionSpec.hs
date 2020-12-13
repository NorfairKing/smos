{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Report.ProjectionSpec
  ( spec,
  )
where

import Cursor.Forest.Gen ()
import Data.Text (Text)
import Smos.Report.Projection
import Smos.Report.Projection.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Text.Megaparsec

spec :: Spec
spec = do
  genValidSpec @Projection
  jsonSpecOnValid @Projection
  genValidSpec @Projectee
  describe "performProjection" $
    it "produces valid projections" $
      forAllValid $
        \s ->
          forAllValid $ \rp -> forAllValid $ \fc -> shouldBeValid $ performProjection s rp fc
  describe "ontoFileP" $ parsesValidSpec ontoFileP
  describe "ontoPropertyP" $ parsesValidSpec ontoPropertyP
  describe "ontoHeaderP" $ parsesValidSpec ontoHeaderP
  describe "ontoStateP" $ parsesValidSpec ontoStateP
  describe "projectionP" $ do
    parsesValidSpec projectionP
    parseJustSpec projectionP "file" OntoFile
    parseJustSpec projectionP "property:effort" $ OntoProperty "effort"
    parseJustSpec projectionP "header" OntoHeader
    parseJustSpec projectionP "state" OntoState
  describe "renderProjection" $ do
    it "produces valid texts" $ producesValidsOnValids renderProjection
    it "renders bys that parse to the same" $
      forAllValid $
        \s -> parseJust projectionP (renderProjection s) s

parseJustSpec :: (Show a, Eq a) => P a -> Text -> a -> Spec
parseJustSpec p s res = it (unwords ["parses", show s, "as", show res]) $ parseJust p s res

parsesValidSpec :: (Show a, Validity a) => P a -> Spec
parsesValidSpec p = it "only parses valid values" $ forAllValid $ parsesValid p

parseJust :: (Show a, Eq a) => P a -> Text -> a -> Expectation
parseJust p s res =
  case parse (p <* eof) "test input" s of
    Left err ->
      expectationFailure $
        unlines ["P failed on input", show s, "with error", errorBundlePretty err]
    Right out -> out `shouldBe` res

parsesValid :: (Show a, Validity a) => P a -> Text -> Expectation
parsesValid p s =
  case parse (p <* eof) "test input" s of
    Left _ -> pure ()
    Right out -> shouldBeValid out
