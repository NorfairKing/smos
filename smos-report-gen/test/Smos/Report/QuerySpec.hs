{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.QuerySpec where

import Data.Text (Text)

import Test.Hspec
import Test.Validity

import Text.Megaparsec

import Smos.Report.Query
import Smos.Report.Query.Gen ()

spec :: Spec
spec = do
    eqSpec @Filter
    genValidSpec @Filter
    describe "parseFilter" $ do
        parsesValidSpec parseFilter
        parseJustSpec parseFilter "tag:work" (FilterHasTag "work")
        parseJustSpec parseFilter "state:NEXT" (FilterTodoState "NEXT")
    describe "parseFilterHasTag" $ parsesValidSpec parseFilterHasTag
    describe "parseFilterTodoState" $ parsesValidSpec parseFilterTodoState
    describe "parseFilterParent" $ parsesValidSpec parseFilterParent
    describe "parseFilterAncestor" $ parsesValidSpec parseFilterAncestor
    describe "parseFilterNot" $ parsesValidSpec parseFilterNot
    describe "parseFilterBinrel" $ parsesValidSpec parseFilterBinRel
    describe "parseFilterOr" $ parsesValidSpec parseFilterOr
    describe "parseFilterAnd" $ parsesValidSpec parseFilterAnd
    describe "renderFilter" $ do
        it "produces valid texts" $ producesValidsOnValids renderFilter
        it "renders filters that parse to the same" $
            forAllValid $ \f -> parseJust parseFilter (renderFilter f) f

parseJustSpec :: (Show a, Eq a) => Parser a -> Text -> a -> Spec
parseJustSpec p s res =
    it (unwords ["parses", show s, "as", show res]) $ parseJust p s res

parseNothingSpec :: (Show a, Eq a) => Parser a -> Text -> Spec
parseNothingSpec p s =
    it (unwords ["fails to parse", show s]) $ parseNothing p s

parsesValidSpec :: (Show a, Eq a, Validity a) => Parser a -> Spec
parsesValidSpec p = it "only parses valid values" $ forAllValid $ parsesValid p

parseJust :: (Show a, Eq a) => Parser a -> Text -> a -> Expectation
parseJust p s res =
    case parse (p <* eof) "test input" s of
        Left err ->
            expectationFailure $
            unlines
                [ "Parser failed on input"
                , show s
                , "with error"
                , parseErrorPretty err
                ]
        Right out -> out `shouldBe` res

parseNothing :: (Show a, Eq a) => Parser a -> Text -> Expectation
parseNothing p s =
    case parse (p <* eof) "test input" s of
        Right v ->
            expectationFailure $
            unlines
                [ "Parser succeeded on input"
                , show s
                , "at parsing"
                , show v
                , "but it should have failed."
                ]
        Left _ -> pure ()

parsesValid :: (Show a, Eq a, Validity a) => Parser a -> Text -> Expectation
parsesValid p s =
    case parse (p <* eof) "test input" s of
        Left _ -> pure ()
        Right out -> shouldBeValid out
