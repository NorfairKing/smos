{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Report.Filter2Spec
  ( spec
  ) where

import Data.Char as Char
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T

import Path

import Test.Hspec
import Test.QuickCheck as QC
import Test.Validity
import Test.Validity.Aeson

import Control.Monad

import Text.Parsec

import Cursor.Forest.Gen ()
import Cursor.Simple.Forest

import Smos.Data

import Smos.Report.Path.Gen ()

import Smos.Report.Filter2
import Smos.Report.Filter2.Gen ()
import Smos.Report.Path
import Smos.Report.Time hiding (P)

spec :: Spec
spec = do
  describe "renderParts" $ it "produces valid texts" $ producesValidsOnValids renderParts
  describe "partP" $ do
    parseSuccessSpec partP ":" PartColumn
    parseSuccessSpec partP "file" (PartPiece (Piece "file"))
    parseSuccessSpec partP "(" (PartParen OpenParen)
    parseSuccessSpec partP ")" (PartParen ClosedParen)
    parseSuccessSpec partP " " PartSpace
    parseSuccessSpec partP "and" (PartBinOp AndOp)
    parseSuccessSpec partP "or" (PartBinOp OrOp)
    parseSuccessSpec partP "a" (PartPiece (Piece "a"))
    parseSuccessSpec partP "o" (PartPiece (Piece "o"))
    parsesValidSpec partP
    it "parses back whatever 'renderPart' renders" $
      forAllValid $ \part ->
        let t = renderPart part
         in case parsePart t of
              Left err -> expectationFailure $ show err
              Right part' -> part' `shouldBe` part
  describe "partsP" $ do
    parseSuccessSpec
      partsP
      "file:side"
      (Parts [PartPiece (Piece "file"), PartColumn, PartPiece (Piece "side")])
    parseSuccessSpec
      partsP
      "(file:side and level:3)"
      (Parts
         [ PartParen OpenParen
         , PartPiece (Piece "file")
         , PartColumn
         , PartPiece (Piece "side")
         , PartSpace
         , PartBinOp AndOp
         , PartSpace
         , PartPiece (Piece "level")
         , PartColumn
         , PartPiece (Piece "3")
         , PartParen ClosedParen
         ])
    parsesValidSpec partsP
    it "parses back whatever 'renderParts' renders" $
      forAllValid $ \parts ->
        let t = renderParts parts
         in case parseParts t of
              Left err -> expectationFailure $ show err
              Right parts' -> parts' `shouldBe` parts
  describe "astP" $ do
    parseSuccessSpec
      astP
      [PartPiece (Piece "file"), PartColumn, PartPiece (Piece "side")]
      (AstUnOp (Piece "file") (AstPiece (Piece "side")))
    parseSuccessSpec
      astP
      [ PartParen OpenParen
      , PartPiece (Piece "file")
      , PartColumn
      , PartPiece (Piece "side")
      , PartSpace
      , PartBinOp AndOp
      , PartSpace
      , PartPiece (Piece "level")
      , PartColumn
      , PartPiece (Piece "3")
      , PartParen ClosedParen
      ]
      (AstBinOp
         (AstUnOp (Piece "file") (AstPiece (Piece "side")))
         AndOp
         (AstUnOp (Piece "level") (AstPiece (Piece "3"))))
  filterArgumentSpec @Time
  filterArgumentSpec @Tag
  filterArgumentSpec @Header
  filterArgumentSpec @TodoState
  filterArgumentSpec @PropertyName
  filterArgumentSpec @PropertyValue
  filterArgumentSpec @TimestampName
  filterArgumentSpec @Timestamp
  filterArgumentSpec @(Path Rel File)
  -- eqSpecOnValid @EntryFilter
  -- genValidSpec @EntryFilter
  -- jsonSpecOnValid @EntryFilter
  -- eqSpecOnValid @(Filter RootedPath)
  -- genValidSpec @(Filter RootedPath)
  -- eqSpecOnValid @(Filter Time)
  -- genValidSpec @(Filter Time)
  -- eqSpecOnValid @(Filter Tag)
  -- genValidSpec @(Filter Tag)
  -- eqSpecOnValid @(Filter Header)
  -- genValidSpec @(Filter Header)
  -- eqSpecOnValid @(Filter TodoState)
  -- genValidSpec @(Filter TodoState)
  -- eqSpecOnValid @(Filter PropertyValue)
  -- genValidSpec @(Filter PropertyValue)
  -- describe "foldFilterAnd" $
  --   it "produces valid results" $
  --   producesValidsOnValids (foldFilterAnd @(RootedPath, ForestCursor Entry))
  -- describe "filterPredicate" $
  --   it "produces valid results" $
  --   producesValidsOnValids2 (filterPredicate @(RootedPath, ForestCursor Entry))

parsesValidSpec ::
     (Show a, Eq a, Validity a, Show s, Stream s Identity m, GenValid s) => Parsec s () a -> Spec
parsesValidSpec parser =
  it "produces valid values whenever parsing succeeds" $
  forAllValid $ \input ->
    let   isRight (Right _) = True
          isRight _ = False
          res = parse parser "test input" input
     in cover 10 (isRight res) "parses" $
        case res of
          Left _ -> pure ()
          Right actual -> shouldBeValid actual

parseSuccessSpec :: (Show a, Eq a, Show s, Stream s Identity m) => Parsec s () a -> s -> a -> Spec
parseSuccessSpec parser input expected =
  it (unwords ["succesfully parses", show input, "into", show expected]) $
  parseSuccess parser input expected

parseSuccess :: (Show a, Eq a, Stream s Identity m) => Parsec s () a -> s -> a -> Expectation
parseSuccess parser input expected =
  case parse parser "test input" input of
    Left pe -> expectationFailure $ show pe
    Right actual -> actual `shouldBe` expected

filterArgumentSpec ::
     forall a. (Show a, Eq a, GenValid a, FilterArgument a)
  => Spec
filterArgumentSpec =
  specify "parseArgument and renderArgument are inverses" $
  forAllValid $ \a -> parseArgument (renderArgument (a :: a)) `shouldBe` Right (a :: a)
