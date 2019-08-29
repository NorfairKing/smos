{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.SorterSpec where

import Data.Text (Text)

import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

import Text.Megaparsec

import Cursor.Forest.Gen ()

import Smos.Report.Path.Gen ()
import Smos.Report.Sorter
import Smos.Report.Sorter.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @Sorter
  genValidSpec @Sorter
  jsonSpecOnValid @Sorter
  describe "sorterOrdering" $
    it "produces valid orderings" $
    forAllValid $ \s ->
      forAllValid $ \(rp1, fc1) ->
        forAllValid $ \(rp2, fc2) -> shouldBeValid $ sorterOrdering s rp1 fc1 rp2 fc2
  describe "byFileP" $ parsesValidSpec byFileP
  describe "byPropertyP" $ parsesValidSpec byPropertyP
  describe "reverseP" $ parsesValidSpec reverseP
  describe "andThenP" $ parsesValidSpec andThenP
  describe "sorterP" $ do
    parsesValidSpec sorterP
    parseJustSpec sorterP "file" ByFile
    parseJustSpec sorterP "property:effort" $ ByProperty "effort"
    parseJustSpec sorterP "reverse:property:effort" $ Reverse (ByProperty "effort")
    parseJustSpec sorterP "(property:effort then file)" $ AndThen (ByProperty "effort") ByFile
  describe "renderSorter" $ do
    it "produces valid texts" $ producesValidsOnValids renderSorter
    it "renders bys that parse to the same" $
      forAllValid $ \s -> parseJust sorterP (renderSorter s) s

parseJustSpec :: (Show a, Eq a) => P a -> Text -> a -> Spec
parseJustSpec p s res = it (unwords ["parses", show s, "as", show res]) $ parseJust p s res

parseNothingSpec :: (Show a, Eq a) => P a -> Text -> Spec
parseNothingSpec p s = it (unwords ["fails to parse", show s]) $ parseNothing p s

parsesValidSpec :: (Show a, Eq a, Validity a) => P a -> Spec
parsesValidSpec p = it "only parses valid values" $ forAllValid $ parsesValid p

parseJust :: (Show a, Eq a) => P a -> Text -> a -> Expectation
parseJust p s res =
  case parse (p <* eof) "test input" s of
    Left err ->
      expectationFailure $ unlines ["P failed on input", show s, "with error", parseErrorPretty err]
    Right out -> out `shouldBe` res

parseNothing :: (Show a, Eq a) => P a -> Text -> Expectation
parseNothing p s =
  case parse (p <* eof) "test input" s of
    Right v ->
      expectationFailure $
      unlines ["P succeeded on input", show s, "at parsing", show v, "but it should have failed."]
    Left _ -> pure ()

parsesValid :: (Show a, Eq a, Validity a) => P a -> Text -> Expectation
parsesValid p s =
  case parse (p <* eof) "test input" s of
    Left _ -> pure ()
    Right out -> shouldBeValid out
