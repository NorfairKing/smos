{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Report.SorterSpec
  ( spec,
  )
where

import Control.Monad
import Cursor.Forest.Gen ()
import Data.Text (Text)
import qualified Data.Text as T
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
  jsonSpec @Sorter
  describe "sorterOrdering" $
    it "produces valid orderings" $
      forAllValid $
        \s ->
          forAllValid $ \(rp1, fc1) ->
            forAllValid $ \(rp2, fc2) -> shouldBeValid $ sorterOrdering s (rp1 :: Path Rel File) fc1 (rp2 :: Path Rel File) fc2
  describe "byFileP" $ parsesValidSpec byFileP
  describe "byHeaderP" $ parsesValidSpec byHeaderP
  describe "byStateP" $ parsesValidSpec byStateP
  describe "byTagP" $ parsesValidSpec byTagP
  describe "byPropertyP" $ parsesValidSpec byPropertyP
  describe "byTimestampP" $ parsesValidSpec byTimestampP
  describe "reverseP" $ parsesValidSpec reverseP
  describe "andThenP" $ parsesValidSpec andThenP
  describe "sorterP" $ do
    parsesValidSpec sorterP
    parseJustSpec sorterP "file" ByFile
    parseJustSpec sorterP "header" ByHeader
    parseJustSpec sorterP "state" ByState
    parseJustSpec sorterP "tag:home" $ ByTag "home"
    parseJustSpec sorterP "property:effort" $ ByProperty "effort"
    parseJustSpec sorterP "property-as-time:timewindow" $ ByPropertyTime "timewindow"
    parseJustSpec sorterP "timestamp:BEGIN" $ ByTimestamp "BEGIN"
    parseJustSpec sorterP "reverse:property:effort" $ Reverse (ByProperty "effort")
    parseJustSpec sorterP "(property:effort then file)" $ AndThen (ByProperty "effort") ByFile
  describe "renderSorter" $ do
    it "produces valid texts" $ producesValid renderSorter
    it "renders sorters that parse to the same" $
      forAllValid $ \s -> parseJust sorterP (renderSorter s) s
  describe "examples" $
    forM_ sorterExamples $ \(description, sorter) ->
      describe (T.unpack description) $ do
        it "is valid" $ shouldBeValid sorter
        it "roundtrips" $
          parseSorter (renderSorter sorter) `shouldBe` Right sorter
        it "roundtrips and back" $
          case parseSorter (renderSorter sorter) of
            Left err -> expectationFailure err
            Right sorter' -> renderSorter sorter' `shouldBe` renderSorter sorter

parseJustSpec :: (Show a, Eq a) => P a -> Text -> a -> Spec
parseJustSpec p s res = it (unwords ["parses", show s, "as", show res]) $ parseJust p s res

parsesValidSpec :: (Show a, Validity a) => P a -> Spec
parsesValidSpec p = do
  it "only parses valid values" $ forAllValid $ parsesValid p
  it "only parses valid values from rendered sorters" $ forAllValid $ \s -> parsesValid p (renderSorter s)

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
