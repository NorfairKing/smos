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

-- import Smos.Report.Filter2.Gen ()
import Smos.Report.Path
import Smos.Report.Time hiding (P)

spec :: Spec
spec = do
  describe "partP" $ do
    parseSuccessSpec partP ":" PartColumn
    parseSuccessSpec partP "file" (PartPiece "file")
    parseSuccessSpec partP "(" (PartParen OpenParen)
    parseSuccessSpec partP ")" (PartParen ClosedParen)
    parseSuccessSpec partP " " PartSpace
    parseSuccessSpec partP "and" (PartBinOp AndOp)
  describe "partsP" $ do
    parseSuccessSpec partsP "file:side" [PartPiece "file", PartColumn, PartPiece "side"]
    parseSuccessSpec
      partsP
      "(file:side and level:3)"
      [ PartParen OpenParen
      , PartPiece "file"
      , PartColumn
      , PartPiece "side"
      , PartSpace
      , PartBinOp AndOp
      , PartSpace
      , PartPiece "level"
      , PartColumn
      , PartPiece "3"
      , PartParen ClosedParen
      ]
  describe "astP" $ do
    parseSuccessSpec
      astP
      [PartPiece "file", PartColumn, PartPiece "side"]
      (AstUnOp (KeyWordPiece KeyWordFile) (AstPiece (FilePiece [relfile|"side"|])))
    parseSuccessSpec
      astP
      [ PartParen OpenParen
      , PartPiece "file"
      , PartColumn
      , PartPiece "side"
      , PartSpace
      , PartBinOp AndOp
      , PartSpace
      , PartPiece "level"
      , PartColumn
      , PartPiece "3"
      , PartParen ClosedParen
      ]
      (AstBinOp AndOp (AstUnOp (KeyWordPiece KeyWordFile) (AstPiece (FilePiece [relfile|"side"|]))) (AstUnOp (KeyWordPiece KeyWordLevel) (AstPiece (LevelPiece 3))))

parseSuccessSpec :: (Show a, Eq a, Show s, Stream s Identity m) => Parsec s () a -> s -> a -> Spec
parseSuccessSpec parser input expected =
  it (unwords ["succesfully parses", show input, "into", show expected]) $
  parseSuccess parser input expected

parseSuccess :: (Show a, Eq a, Stream s Identity m) => Parsec s () a -> s -> a -> Expectation
parseSuccess parser input expected =
  case parse parser "test input" input of
    Left pe -> expectationFailure $ show pe
    Right actual -> actual `shouldBe` expected
