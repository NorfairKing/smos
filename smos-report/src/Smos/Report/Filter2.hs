{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Report.Filter2 where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Char as Char
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Data.Void
import Path
import Text.Read

import Control.Arrow
import Control.Monad

import Lens.Micro

import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Error
import Text.Parsec.Prim

import Cursor.Simple.Forest
import Cursor.Simple.Tree

import Smos.Data

import Smos.Report.Path
import Smos.Report.Time hiding (P)

data Paren
  = OpenParen
  | ClosedParen
  deriving (Show, Eq, Generic)

instance Validity Paren

data BinOp
  = AndOp
  | OrOp
  deriving (Show, Eq, Generic)

instance Validity BinOp

data Part
  = PartParen Paren
  | PartSpace
  | PartPiece Text
  | PartBinOp BinOp
  deriving (Show, Eq, Generic)

instance Validity Part where
  validate t =
    mconcat
      [ genericValidate t
      , case t of
          PartPiece text ->
            declare "The characters are restricted" $
            all (\c -> not (Char.isSpace c) && Char.isPrint c && c /= '(' && c /= ')' && c /= ':') $
            T.unpack text
          _ -> valid
      ]

parseParts :: Text -> Maybe [Part]
parseParts = undefined

data KeyWord
  = KeyWordFile
  | KeyWordTime
  | KeyWordHeader
  | KeyWordTodoState
  | KeyWordProperties
  | KeyWordTags
  | KeyWordLevel
  | KeyWordParent
  | KeyWordAncestor
  | KeyWordChild
  | KeyWordLegacy
  | KeyWordAny
  | KeyWordAll
  | KeyWordHas
  | KeyWordVal
  | KeyWordFst
  | KeyWordSnd
  | KeyWordMaybe
  | KeyWordSub
  | KeyWordOrd
  | KeyWordNot
  deriving (Show, Eq, Generic)

instance Validity KeyWord

data Comparison
  = LTC
  | LEC
  | EQC
  | GEC
  | GTC
  deriving (Show, Eq, Ord, Generic)

instance Validity Comparison

data Piece
  = KeyWordPiece KeyWord
  | BoolPiece Bool
  | TimePiece Time
  | FilePiece (Path Rel File)
  | HeaderPiece Header
  | TodoStatePiece TodoState
  | PropertyNamePiece PropertyName
  | PropertyValuePiece PropertyValue
  | TagPiece Tag
  | LevelPiece Word
  | ComparisonPiece Comparison
  deriving (Show, Eq, Generic)

instance Validity Piece

data Ast
  = AstBinOp BinOp Ast Ast
  | AstUnOp Piece Ast
  | AstPiece Piece
  deriving (Show, Eq, Generic)

instance Validity Ast

parseAst :: [Part] -> Either ParseError Ast
parseAst = parse astP "ast"

type P = Parsec [Part] ()

astP :: P Ast
astP = undefined

part :: Monad m => (Part -> Bool) -> ParsecT [Part] u m Part
part func = tokenPrim showPart nextPos testPart
  where
    showPart = show
    testPart x =
      if func x
        then Just x
        else Nothing
    nextPos pos x _ = updatePosPart pos x
      where
        updatePosPart :: SourcePos -> Part -> SourcePos
        updatePosPart sp p =
          let l =
                case p of
                  PartParen _ -> 1
                  PartSpace -> 1
                  PartPiece t -> T.length t
                  PartBinOp bo ->
                    case bo of
                      AndOp -> 3
                      OrOp -> 2
           in incSourceColumn sp l
