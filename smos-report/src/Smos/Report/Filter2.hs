{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Set (Set)
import qualified Data.Text as T
import Data.Text (Text)
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

import Smos.Report.Comparison
import Smos.Report.Path
import Smos.Report.Time hiding (P)

data Paren
  = OpenParen
  | ClosedParen
  deriving (Show, Eq, Generic)

instance Validity Paren

renderParen :: Paren -> Char
renderParen =
  \case
    OpenParen -> '('
    ClosedParen -> ')'

data BinOp
  = AndOp
  | OrOp
  deriving (Show, Eq, Generic)

instance Validity BinOp

renderBinOp :: BinOp -> Text
renderBinOp =
  \case
    AndOp -> "and"
    OrOp -> "or"

newtype Piece =
  Piece
    { pieceText :: Text
    }
  deriving (Show, Eq, Generic)

instance Validity Piece where
  validate p@(Piece t) =
    mconcat
      [ genericValidate p
      , declare "The piece is not empty" $ not $ T.null t
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

renderPart :: Part -> Text
renderPart =
  \case
    PartParen p -> T.singleton $ renderParen p
    PartSpace -> " "
    PartColumn -> ":"
    PartPiece p -> pieceText p
    PartBinOp bo -> renderBinOp bo

newtype Parts =
  Parts
    { unParts :: [Part]
    }
  deriving (Show, Eq, Generic)

instance Validity Parts where
  validate p@(Parts ps) =
    mconcat
      [ genericValidate p
      , declare "There are no two unfitting consequtive pieces" $
        let go [] = True
            go [_] = True
            go (p1:p2:ps) =
              let b =
                    case p1 of
                      PartPiece _ ->
                        case p2 of
                          PartPiece _ -> False
                          PartBinOp _ -> False
                          _ -> True
                      _ -> True
               in b && go (p2 : ps)
         in go ps
      ]

renderParts :: Parts -> Text
renderParts = T.concat . map renderPart . unParts

parseParts :: Text -> Either ParseError Parts
parseParts = parse partsP "parts"

partsP :: TP Parts
partsP = Parts <$> many partP

parsePart :: Text -> Either ParseError Part
parsePart = parse partP "part"

partP :: TP Part
partP =
  choice
    [ void (char ':') >> pure PartColumn
    , void (char '(') >> pure (PartParen OpenParen)
    , void (char ')') >> pure (PartParen ClosedParen)
    , void (char ' ') >> pure PartSpace
    , try $ void (string "and") >> pure (PartBinOp AndOp)
    , try $ void (string "or") >> pure (PartBinOp OrOp)
    , PartPiece . Piece . T.pack <$>
      many1
        (satisfy (\c -> not (Char.isSpace c) && Char.isPrint c && c /= '(' && c /= ')' && c /= ':'))
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

data Ast
  = AstBinOp Ast BinOp Ast
  | AstUnOp Piece Ast
  | AstPiece Piece
  deriving (Show, Eq, Generic)

instance Validity Ast

renderAst :: Ast -> Parts
renderAst = Parts . go
  where
    go =
      \case
        AstPiece p -> [PartPiece p]
        AstUnOp p a -> PartPiece p : PartColumn : go a
        AstBinOp a1 bo a2 ->
          concat
            [ [PartParen OpenParen]
            , go a1
            , [PartSpace, PartBinOp bo, PartSpace]
            , go a2
            , [PartParen ClosedParen]
            ]

parseAst :: Parts -> Either ParseError Ast
parseAst = parse astP "ast" . unParts

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

--
--
--
--
--
type EntryFilter = Filter (RootedPath, ForestCursor Entry)

data SmosFileAtPath =
  SmosFileAtPath
    { smosFilePath :: RootedPath
    , smosFileFile :: SmosFile
    }
  deriving (Show, Eq, Generic)

forestCursorCurrent :: ForestCursor a -> a
forestCursorCurrent fc = fc ^. forestCursorSelectedTreeL . treeCursorCurrentL

forestCursorChildren :: ForestCursor a -> [ForestCursor a]
forestCursorChildren a =
  mapMaybe
    (\i -> a & forestCursorSelectBelowAtPos i)
    (let CNode _ cf = rebuildTreeCursor $ a ^. forestCursorSelectedTreeL
      in [0 .. length (rebuildCForest cf) - 1])

forestCursorLevel :: ForestCursor a -> Word
forestCursorLevel fc = go' $ fc ^. forestCursorSelectedTreeL
  where
    go' tc =
      case tc ^. treeCursorAboveL of
        Nothing -> 0
        Just tc' -> 1 + goA' tc'
    goA' ta =
      case treeAboveAbove ta of
        Nothing -> 0
        Just ta' -> 1 + goA' ta'

data Filter a where
  FilterFile :: Path Rel File -> Filter RootedPath
  -- Parsing filters
  FilterPropertyTime :: Filter (Maybe Time) -> Filter PropertyValue
  -- Entry mapping filters
  FilterEntryHeader :: Filter Header -> Filter Entry
  FilterEntryTodoState :: Filter (Maybe TodoState) -> Filter Entry
  FilterEntryProperties :: Filter (Map PropertyName PropertyValue) -> Filter Entry
  FilterEntryTags :: Filter (Set Tag) -> Filter Entry
  -- Cursor-related filters
  FilterWithinCursor :: Filter a -> Filter (ForestCursor a)
  FilterLevel :: Word -> Filter (ForestCursor a)
  FilterParent :: Filter (ForestCursor a) -> Filter (ForestCursor a)
  FilterAncestor :: Filter (ForestCursor a) -> Filter (ForestCursor a)
  FilterChild :: Filter (ForestCursor a) -> Filter (ForestCursor a)
  FilterLegacy :: Filter (ForestCursor a) -> Filter (ForestCursor a)
  -- List filters
  FilterAny :: Filter a -> Filter (Set a)
  FilterAll :: Filter a -> Filter (Set a)
  -- Map filters
  FilterMapHas :: (Validity k, Show k, Ord k, FilterArgument k) => k -> Filter (Map k v)
  FilterMapVal
    :: (Validity k, Show k, Ord k, FilterArgument k) => k -> Filter (Maybe v) -> Filter (Map k v)
  -- Tuple filters
  FilterFst :: Filter a -> Filter (a, b)
  FilterSnd :: Filter b -> Filter (a, b)
  -- Maybe filters
  FilterMaybe :: Bool -> Filter a -> Filter (Maybe a)
  -- Comparison filters
  FilterSub :: (Validity a, Show a, Ord a, FilterArgument a, FilterSubString a) => a -> Filter a
  FilterOrd
    :: (Validity a, Show a, Ord a, FilterArgument a, FilterOrd a) => Comparison -> a -> Filter a
  -- Boolean filters
  FilterNot :: Filter a -> Filter a
  FilterAnd :: Filter a -> Filter a -> Filter a
  FilterOr :: Filter a -> Filter a -> Filter a

class FilterSubString a where
  filterSubString :: a -> a -> Bool

instance FilterSubString Header where
  filterSubString = T.isInfixOf `on` renderArgument

instance FilterSubString TodoState where
  filterSubString = T.isInfixOf `on` renderArgument

instance FilterSubString PropertyValue where
  filterSubString = T.isInfixOf `on` renderArgument

instance FilterSubString Tag where
  filterSubString = T.isInfixOf `on` renderArgument

class FilterOrd a

instance FilterOrd Word

instance FilterOrd Time

instance FilterOrd Timestamp

class FilterArgument a where
  renderArgument :: a -> Text
  parseArgument :: Text -> Either String a

instance FilterArgument Word where
  renderArgument = T.pack . show
  parseArgument = maybe (Left "Invalid word") Right . readMaybe . T.unpack

instance FilterArgument (Path Rel File) where
  renderArgument = T.pack . fromRelFile
  parseArgument = left show . parseRelFile . T.unpack

instance FilterArgument Header where
  renderArgument = headerText
  parseArgument = parseHeader

instance FilterArgument TodoState where
  renderArgument = todoStateText
  parseArgument = parseTodoState

instance FilterArgument PropertyName where
  renderArgument = propertyNameText
  parseArgument = parsePropertyName

instance FilterArgument PropertyValue where
  renderArgument = propertyValueText
  parseArgument = parsePropertyValue

instance FilterArgument TimestampName where
  renderArgument = timestampNameText
  parseArgument = parseTimestampName

instance FilterArgument Timestamp where
  renderArgument = timestampText
  parseArgument = maybe (Left "Invalid timestamp") Right . parseTimestampText

instance FilterArgument Tag where
  renderArgument = tagText
  parseArgument = parseTag

instance FilterArgument Time where
  renderArgument = renderTime
  parseArgument = parseTime

instance Validity (Filter a) where
  validate f =
    let validateArgument a =
          mconcat
            [ validate a
            , declare "The characters are restricted" $
              all (\c -> not (Char.isSpace c) && Char.isPrint c && c /= '(' && c /= ')' && c /= ':') $
              T.unpack $
              renderArgument a
            , declare "The argument is not empty" $ not $ T.null $ renderArgument a
            ]
     in case f of
          FilterFile s ->
            mconcat
              [ validate s
              , declare "The filenames are restricted" $
                all (\c -> not (Char.isSpace c) && c /= ')' && not (isUtf16SurrogateCodePoint c)) $
                fromRelFile s
              ]
          FilterPropertyTime f' -> validate f'
          FilterEntryHeader f' -> validate f'
          FilterEntryTodoState f' -> validate f'
          FilterEntryProperties f' -> validate f'
          FilterEntryTags f' -> validate f'
          FilterWithinCursor f' -> validate f'
          FilterLevel w -> validate w
          FilterParent f' -> validate f'
          FilterAncestor f' -> validate f'
          FilterChild f' -> validate f'
          FilterLegacy f' -> validate f'
          FilterAny f' -> validate f'
          FilterAll f' -> validate f'
          FilterMapHas a -> mconcat [validate a, validateArgument a]
          FilterMapVal a f' -> mconcat [validate a, validateArgument a, validate f']
          FilterFst f' -> validate f'
          FilterSnd f' -> validate f'
          FilterMaybe b f' -> mconcat [validate b, validate f']
          FilterSub a -> mconcat [validate a, validateArgument a]
          FilterOrd o a -> mconcat [validate o, validateArgument a]
          FilterNot f' -> validate f'
          FilterAnd f1 f2 -> mconcat [validate f1, validate f2]
          FilterOr f1 f2 -> mconcat [validate f1, validate f2]

deriving instance Show (Filter a)

deriving instance Eq (Filter a)

deriving instance Ord (Filter a)

instance FromJSON (Filter (RootedPath, ForestCursor Entry)) where
  parseJSON =
    withText "EntryFilter" $ \t ->
      case parseEntryFilter t of
        Left err -> fail $ unwords ["Could not parse EntryFilter:", err]
        Right f -> pure f

instance ToJSON (Filter a) where
  toJSON = toJSON . renderFilter

foldFilterAnd :: NonEmpty (Filter a) -> Filter a
foldFilterAnd = foldl1 FilterAnd

filterPredicate :: Filter a -> a -> Bool
filterPredicate = go
  where
    go :: forall a. Filter a -> a -> Bool
    go f a =
      let goF f' = go f' a
          goProj :: forall b. (a -> b) -> Filter b -> Bool
          goProj func f' = go f' $ func a
       in case f of
            FilterFile rp -> fromRelFile rp `isInfixOf` fromAbsFile (resolveRootedPath a)
            -- Parsing filters
            FilterPropertyTime f' -> goProj (time . propertyValueText) f'
            -- Entry mapping filters
            FilterEntryHeader f' -> goProj entryHeader f'
            FilterEntryTodoState f' -> goProj entryState f'
            FilterEntryProperties f' -> goProj entryProperties f'
            FilterEntryTags f' -> goProj entryTags f'
            -- Cursor-related filters
            FilterWithinCursor f' -> go f' (forestCursorCurrent a)
            FilterLevel l -> l == forestCursorLevel a
            FilterAncestor f' ->
              maybe False (\fc_ -> go f' fc_ || go f fc_) (forestCursorSelectAbove a) || go f' a
            FilterLegacy f' ->
              any (\fc_ -> go f' fc_ || go f fc_) (forestCursorChildren a) || go f' a
            FilterParent f' -> maybe False (go f') (forestCursorSelectAbove a)
            FilterChild f' -> any (go f') (forestCursorChildren a)
            -- List filters
            FilterAny f' -> any (go f') a
            FilterAll f' -> all (go f') a
            -- Map filters
            FilterMapHas k -> M.member k a
            FilterMapVal k f' -> goProj (M.lookup k) f'
            -- Tuple filters
            FilterFst f' -> goProj fst f'
            FilterSnd f' -> goProj snd f'
            -- Maybe filters
            FilterMaybe b f' -> maybe b (go f') a
            -- Comparison filters
            FilterSub t -> t `filterSubString` a
            FilterOrd o a' -> comparisonFunc o a a'
            -- Boolean filters
            FilterNot f' -> not $ goF f'
            FilterAnd f1 f2 -> goF f1 && goF f2
            FilterOr f1 f2 -> goF f1 || goF f2

renderFilter :: Filter a -> Text
renderFilter = go
  where
    go :: Filter a -> Text
    go f =
      let p t1 t2 = t1 <> ":" <> t2
          p1 t f' = p t $ go f'
          p2 f1 o f2 = T.concat ["(", renderFilter f1, " ", o, " ", renderFilter f2, ")"]
       in case f of
            FilterFile rp -> p "file" $ renderArgument rp
            FilterPropertyTime f' -> p1 "time" f'
                -- Entry mapping filters
            FilterEntryHeader f' -> p1 "header" f'
            FilterEntryTodoState f' -> p1 "state" f'
            FilterEntryProperties f' -> p1 "property" f'
            FilterEntryTags f' -> p1 "tag" f'
                -- Cursor-related filters
            FilterWithinCursor f' -> go f'
            FilterLevel l -> p "level" $ renderArgument l
            FilterAncestor f' -> p1 "ancestor" f'
            FilterLegacy f' -> p1 "legacy" f'
            FilterParent f' -> p1 "parent" f'
            FilterChild f' -> p1 "child" f'
                -- List filters
            FilterAny f' -> go f'
            FilterAll f' -> p1 "all" f'
                -- Map filters
            FilterMapHas k -> p "has" $ renderArgument k
            FilterMapVal k f' -> p "val" $ p1 (renderArgument k) f'
                -- Tuple filters
            FilterFst f' -> go f'
            FilterSnd f' -> go f'
                -- Maybe filters
            FilterMaybe b f' ->
              if b
                then p1 "maybe-true" f'
                else go f' -- p1 "maybe-false" f'
                -- Comparison filters
            FilterSub t -> renderArgument t
            FilterOrd o a -> p (renderComparison o) (renderArgument a)
                -- Boolean filters
            FilterNot f' -> p1 "not" f'
            FilterOr f1 f2 -> p2 f1 "or" f2
            FilterAnd f1 f2 -> p2 f1 "and" f2

parseEntryFilter :: Text -> Either String EntryFilter
parseEntryFilter = undefined

data DerivationError =
  DerivationError
  deriving (Show, Eq, Generic)

instance Validity DerivationError

deriveFilterFromAst :: Ast -> Either DerivationError EntryFilter
deriveFilterFromAst = undefined
