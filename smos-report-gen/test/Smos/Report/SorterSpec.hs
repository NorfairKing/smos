{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Report.SorterSpec
  ( spec,
  )
where

import Cursor.Forest.Gen ()
import Data.Text (Text)
import Path
import Smos.Report.Sorter
import Smos.Report.Sorter.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Text.Megaparsec

spec :: Spec
spec = do
  genValidSpec @Sorter
  jsonSpecOnValid @Sorter
  describe "sorterOrdering" $
    it "produces valid orderings" $
      forAllValid $
        \s ->
          forAllValid $ \(rp1, fc1) ->
            forAllValid $ \(rp2, fc2) -> shouldBeValid $ sorterOrdering s (rp1 :: Path Rel File) fc1 (rp2 :: Path Rel File) fc2
  describe "byFileP" $ parsesValidSpec byFileP
  describe "byPropertyP" $ parsesValidSpec byPropertyP
  describe "reverseP" $ parsesValidSpec reverseP
  describe "andThenP" $ parsesValidSpec andThenP
  describe "sorterP" $ do
    parsesValidSpec sorterP
    parseJustSpec sorterP "file" ByFile
    parseJustSpec sorterP "property:effort" $ ByProperty "effort"
    parseJustSpec sorterP "property-as-time:timewindow" $ ByPropertyTime "timewindow"
    parseJustSpec sorterP "reverse:property:effort" $ Reverse (ByProperty "effort")
    parseJustSpec sorterP "(property:effort then file)" $ AndThen (ByProperty "effort") ByFile
  describe "renderSorter" $ do
    it "produces valid texts" $ producesValidsOnValids renderSorter
    it "renders bys that parse to the same" $
      forAllValid $
        \s -> parseJust sorterP (renderSorter s) s

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
