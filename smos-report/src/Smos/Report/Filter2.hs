{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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
import Text.Read (readMaybe)

import Control.Arrow
import Control.Monad

import Lens.Micro

import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

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

newtype Piece =
  Piece
    { pieceText :: Text
    }
  deriving (Show, Eq, Generic)

instance Validity Piece where
  validate p@(Piece t) =
    mconcat
      [ genericValidate p
      , declare "The characters are restricted" $
        all (\c -> not (Char.isSpace c) && Char.isPrint c && c /= '(' && c /= ')' && c /= ':') $
        T.unpack t
      ]

data Part
  = PartParen Paren
  | PartSpace
  | PartColumn
  | PartPiece Piece
  | PartBinOp BinOp
  deriving (Show, Eq, Generic)

instance Validity Part

parseParts :: Text -> Either ParseError [Part]
parseParts = parse partsP "parts"

partsP :: TP [Part]
partsP = many partP

partP :: TP Part
partP =
  choice
    [ void (char ':') >> pure PartColumn
    , void (char '(') >> pure (PartParen OpenParen)
    , void (char ')') >> pure (PartParen ClosedParen)
    , void (char ' ') >> pure PartSpace
    , void (string "and") >> pure (PartBinOp AndOp)
    , void (string "or") >> pure (PartBinOp OrOp)
    , PartPiece . Piece . T.pack <$> many1 (noneOf ":() ")
    ]

type TP = Parsec Text ()

type PP = Parsec [Part] ()

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
                  PartColumn -> 1
                  PartPiece (Piece t) -> T.length t
                  PartBinOp bo ->
                    case bo of
                      AndOp -> 3
                      OrOp -> 2
           in incSourceColumn sp l

pieceP :: PP Piece
pieceP = do
  p <- part (const True)
  case p of
    PartPiece t -> pure t
    _ -> fail "expected piece"

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

parseKeyword :: Text -> Maybe KeyWord
parseKeyword =
  \case
    "file" -> Just KeyWordFile
    "time" -> Just KeyWordTime
    "header" -> Just KeyWordHeader
    "todoState" -> Just KeyWordTodoState
    "properties" -> Just KeyWordProperties
    "tags" -> Just KeyWordTags
    "level" -> Just KeyWordLevel
    "parent" -> Just KeyWordParent
    "ancestor" -> Just KeyWordAncestor
    "child" -> Just KeyWordChild
    "legacy" -> Just KeyWordLegacy
    "any" -> Just KeyWordAny
    "all" -> Just KeyWordAll
    "has" -> Just KeyWordHas
    "val" -> Just KeyWordVal
    "fst" -> Just KeyWordFst
    "snd" -> Just KeyWordSnd
    "maybe" -> Just KeyWordMaybe
    "sub" -> Just KeyWordSub
    "ord" -> Just KeyWordOrd
    "not" -> Just KeyWordNot
    _ -> Nothing

renderKeyWord :: KeyWord -> Text
renderKeyWord =
  \case
    KeyWordFile -> "file"
    KeyWordTime -> "time"
    KeyWordHeader -> "header"
    KeyWordTodoState -> "todoState"
    KeyWordProperties -> "properties"
    KeyWordTags -> "tags"
    KeyWordLevel -> "level"
    KeyWordParent -> "parent"
    KeyWordAncestor -> "ancestor"
    KeyWordChild -> "child"
    KeyWordLegacy -> "legacy"
    KeyWordAny -> "any"
    KeyWordAll -> "all"
    KeyWordHas -> "has"
    KeyWordVal -> "val"
    KeyWordFst -> "fst"
    KeyWordSnd -> "snd"
    KeyWordMaybe -> "maybe"
    KeyWordSub -> "sub"
    KeyWordOrd -> "ord"
    KeyWordNot -> "not"

anyKeyWordP :: PP KeyWord
anyKeyWordP = do
  Piece t <- pieceP
  case parseKeyword t of
    Just kw -> pure kw
    Nothing -> fail $ "not a keyword: " <> T.unpack t

keyWordP :: KeyWord -> PP ()
keyWordP kw = do
  Piece t <- pieceP
  case parseKeyword t of
    Just kw'
      | kw' == kw' -> pure ()
    _ -> fail $ "expected keyword: " <> T.unpack (renderKeyWord kw) <> " got: " <> T.unpack t

data Comparison
  = LTC
  | LEC
  | EQC
  | GEC
  | GTC
  deriving (Show, Eq, Ord, Generic)

instance Validity Comparison

parseComparison :: Text -> Maybe Comparison
parseComparison =
  \case
    "lt" -> Just LTC
    "le" -> Just LEC
    "eq" -> Just EQC
    "ge" -> Just GEC
    "gt" -> Just GTC
    _ -> Nothing

renderComparison :: Comparison -> Text
renderComparison =
  \case
    LTC -> "lt"
    LEC -> "le"
    EQC -> "eq"
    GEC -> "ge"
    GTC -> "gt"

comparisonP :: PP Comparison
comparisonP = do
  Piece t <- pieceP
  case parseComparison t of
    Just kw -> pure kw
    Nothing -> fail $ "not a comparison:" <> T.unpack t

-- data Piece
--   = KeyWordPiece KeyWord
--   | BoolPiece Bool
--   | TimePiece Time
--   | FilePiece (Path Rel File)
--   | HeaderPiece Header
--   | TodoStatePiece TodoState
--   | PropertyNamePiece PropertyName
--   | PropertyValuePiece PropertyValue
--   | TagPiece Tag
--   | LevelPiece Word
--   | ComparisonPiece Comparison
--   deriving (Show, Eq, Generic)
--
-- instance Validity Piece
data Ast
  = AstBinOp Ast BinOp Ast
  | AstUnOp Piece Ast
  | AstPiece Piece
  deriving (Show, Eq, Generic)

instance Validity Ast

parseAst :: [Part] -> Either ParseError Ast
parseAst = parse astP "ast"

astP :: PP Ast
astP = choice [try astBinOpP, try astUnOpP, AstPiece <$> pieceP]

astBinOpP :: PP Ast
astBinOpP = do
  parenP OpenParen
  void $ optional spaceP
  a1 <- astP
  void $ optional spaceP
  bo <- binOpP
  void $ optional spaceP
  a2 <- astP
  void $ optional spaceP
  parenP ClosedParen
  pure $ AstBinOp a1 bo a2

binOpP :: PP BinOp
binOpP = do
  PartBinOp bo <-
    part
      (\case
         PartBinOp _ -> True
         _ -> False)
  pure bo

astUnOpP :: PP Ast
astUnOpP = do
  p <- pieceP
  columnP
  a <- astP
  pure $ AstUnOp p a

parenP :: Paren -> PP ()
parenP p =
  void $
  part
    (\case
       PartParen p' -> p' == p
       _ -> False)

columnP :: PP ()
columnP =
  void $
  part
    (\case
       PartColumn -> True
       _ -> False)

spaceP :: PP ()
spaceP =
  void $
  part
    (\case
       PartSpace -> True
       _ -> False)
