{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.FilterSpec
  ( spec
  ) where

import Data.Char as Char
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Test.Hspec
import Test.QuickCheck as QC
import Test.Validity
import Test.Validity.Aeson

import Text.Megaparsec

import Cursor.Forest.Gen ()

import Smos.Report.Path.Gen ()

import Smos.Report.Filter
import Smos.Report.Filter.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @Filter
  genValidSpec @Filter
  jsonSpecOnValid @Filter
  describe "foldFilterAnd" $ it "produces valid results" $ producesValidsOnValids foldFilterAnd
  describe "filterPredicate" $ it "produces valid results" $ producesValidsOnValids3 filterPredicate
  describe "filterP" $ do
    parsesValidSpec filterP filterText
    parseJustSpec filterP "tag:work" (FilterHasTag "work")
    parseJustSpec filterP "state:NEXT" (FilterTodoState "NEXT")
    parseJustSpec filterP "level:5" (FilterLevel 5)
    parseJustSpec filterP "exact-property:effort:30m" (FilterExactProperty "effort" "30m")
    parseJustSpec filterP "has-property:effort" (FilterHasProperty "effort")
  describe "filterHasTagP" $ parsesValidSpec filterHasTagP tagText
  describe "filterTodoStateP" $ parsesValidSpec filterTodoStateP todoStateText
  describe "filterFileP" $ parsesValidSpec filterFileP fileText
  describe "filterLevelP" $ parsesValidSpec filterLevelP levelText
  describe "filterHeaderP" $ parsesValidSpec filterHeaderP headerText
  describe "filterParentP" $ parsesValidSpec filterParentP parentText
  describe "filterAncestorP" $ parsesValidSpec filterAncestorP ancestorText
  describe "filterChildP" $ parsesValidSpec filterChildP childText
  describe "filterLegacyP" $ parsesValidSpec filterLegacyP legacyText
  describe "filterNotP" $ parsesValidSpec filterNotP notText
  describe "filterBinrelP" $ parsesValidSpec filterBinRelP binRelText
  describe "filterOrP" $ parsesValidSpec filterOrP orText
  describe "filterAndP" $ parsesValidSpec filterAndP andText
  describe "filterExactPropertyP" $ parsesValidSpec filterExactPropertyP exactPropertyText
  describe "filterHasPropertyP" $ parsesValidSpec filterHasPropertyP hasPropertyText
  describe "renderFilter" $ do
    it "produces valid texts" $ producesValidsOnValids renderFilter
    it "renders filters that parse to the same" $
      forAllValid $ \f -> parseJust filterP (renderFilter f) f
  describe "filterCompleter" $ do
    let c s ss =
          it (unwords ["completes", show s, "to", show ss]) $
          sort (filterCompleter s) `shouldBe` sort ss
        cp s ss = xdescribe "Not implemented yet." $ c s ss
    c "t" ["tag:"]
    c "s" ["state:"]
    c "n" ["not:"]
    c "p" ["parent:", "property:"]
    c
      "not"
      [ "not:tag:"
      , "not:state:"
      , "not:file:"
      , "not:level:"
      , "not:property:"
      , "not:parent:"
      , "not:ancestor:"
      , "not:child:"
      , "not:legacy:"
      , "not:not:"
      ]
    c
      "not:"
      [ "not:tag:"
      , "not:state:"
      , "not:file:"
      , "not:level:"
      , "not:property:"
      , "not:parent:"
      , "not:ancestor:"
      , "not:child:"
      , "not:legacy:"
      , "not:not:"
      ]
    c "tag" ["tag:out", "tag:online", "tag:personal", "tag:offline", "tag:toast", "tag:work"]
    c "tag:" ["tag:out", "tag:online", "tag:personal", "tag:offline", "tag:toast", "tag:work"]
    cp "tag:h" ["tag:home"]
    cp "state:N" ["state:NEXT"]

filterText :: Gen Text
filterText =
  oneof
    [ tagText
    , todoStateText
    , fileText
    , levelText
    , headerText
    , hasPropertyText
    , exactPropertyText
    , parentText
    , ancestorText
    , childText
    , legacyText
    , notText
    , binRelText
    ]

tagText :: Gen Text
tagText =
  textPieces
    [ pure "tag:"
    , T.pack <$>
      genListOf
        (genValid `suchThat`
         (\c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c)))
    ]

todoStateText :: Gen Text
todoStateText =
  textPieces
    [ pure "state:"
    , T.pack <$>
      genListOf
        (genValid `suchThat`
         (\c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c)))
    ]

fileText :: Gen Text
fileText = textPieces [pure "file:", genValid]

levelText :: Gen Text
levelText = textPieces [pure "level:", T.pack . show <$> (genValid :: Gen Int)]

headerText :: Gen Text
headerText =
  textPieces
    [ pure "header:"
    , T.pack <$>
      genListOf
        (genValid `suchThat`
         (\c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c)))
    ]

parentText :: Gen Text
parentText = textPieces [pure "parent:", filterText]

ancestorText :: Gen Text
ancestorText = textPieces [pure "ancestor:", filterText]

childText :: Gen Text
childText = textPieces [pure "child:", filterText]

legacyText :: Gen Text
legacyText = textPieces [pure "legacy:", filterText]

notText :: Gen Text
notText = textPieces [pure "not:", filterText]

binRelText :: Gen Text
binRelText = textPieces [pure "(", oneof [orText, andText], pure ")"]

orText :: Gen Text
orText = textPieces [filterText, pure " or ", filterText]

andText :: Gen Text
andText = textPieces [filterText, pure " and ", filterText]

exactPropertyText :: Gen Text
exactPropertyText =
  textPieces [pure "exact-property:", propertyNameText, pure ":", propertyValueText]

hasPropertyText :: Gen Text
hasPropertyText = textPieces [pure "has-property:", propertyNameText]

-- These don't match exactly, but they're a good start.
propertyNameText :: Gen Text
propertyNameText =
  T.pack <$>
  genListOf
    (genValid `suchThat`
     (\c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c)))

-- These don't match exactly, but they're a good start.
propertyValueText :: Gen Text
propertyValueText =
  T.pack <$>
  genListOf
    (genValid `suchThat`
     (\c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c)))

textPieces :: [Gen Text] -> Gen Text
textPieces = fmap T.concat . sequenceA

parseJustSpec :: (Show a, Eq a) => P a -> Text -> a -> Spec
parseJustSpec p s res = it (unwords ["parses", show s, "as", show res]) $ parseJust p s res

parsesValidSpec :: (Show a, Eq a, Validity a) => P a -> Gen Text -> Spec
parsesValidSpec p gen = it "only parses valid values" $ forAll gen $ parsesValid p

parseJust :: (Show a, Eq a) => P a -> Text -> a -> Expectation
parseJust p s res =
  case parse (p <* eof) "test input" s of
    Left err ->
      expectationFailure $
      unlines ["P failed on input", show s, "with error", errorBundlePretty err]
    Right out -> out `shouldBe` res

parsesValid :: (Show a, Eq a, Validity a) => P a -> Text -> Property
parsesValid p s =
  checkCoverage $
  let (useful, ass) =
        case parse (p <* eof) "test input" s of
          Left _ -> (False, pure () :: IO ())
          Right out -> (True, shouldBeValid out)
   in cover 10.0 useful "useful" $ property ass
