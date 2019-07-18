{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Filter where

import GHC.Generics (Generic)

import Data.Char as Char
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Data.Void
import Path

import Control.Monad

import Lens.Micro

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Cursor.Simple.Forest
import Cursor.Simple.Tree

import Smos.Data

import Smos.Report.Path

data Filter
  = FilterHasTag Tag
  | FilterTodoState TodoState
  | FilterFile (Path Rel File) -- Substring of the filename
  | FilterLevel Word -- The level of the entry in the tree (0 is top)
  | FilterProperty PropertyFilter
  | FilterParent Filter -- Match direct parent
  | FilterAncestor Filter -- Match parent or parent of parents recursively
  | FilterChild Filter -- Match any direct child
  | FilterLegacy Filter -- Match any direct child or their children
  | FilterNot Filter
  | FilterAnd Filter Filter
  | FilterOr Filter Filter
  deriving (Show, Eq, Generic)

instance Validity Filter where
  validate f =
    mconcat
      [ genericValidate f
      , case f of
          FilterFile s ->
            declare "The filenames are restricted" $ all (\c -> not (Char.isSpace c) && c /= ')') $
            fromRelFile s
          _ -> valid
      ]

data PropertyFilter
  = ExactProperty PropertyName PropertyValue
  | HasProperty PropertyName
  deriving (Show, Eq, Generic)

instance Validity PropertyFilter

foldFilterAnd :: NonEmpty Filter -> Filter
foldFilterAnd = foldl1 FilterAnd

filterPredicate :: Filter -> RootedPath -> ForestCursor Entry -> Bool
filterPredicate f_ rp = go f_
  where
    go f fc =
      let parent_ :: Maybe (ForestCursor Entry)
          parent_ = fc & forestCursorSelectedTreeL treeCursorSelectAbove
          children_ :: [ForestCursor Entry]
          children_ =
            mapMaybe
              (\i -> fc & forestCursorSelectBelowAtPos i)
              (let CNode _ cf = rebuildTreeCursor $ fc ^. forestCursorSelectedTreeL
                in [0 .. length (rebuildCForest cf) - 1])
          cur :: Entry
          cur = fc ^. forestCursorSelectedTreeL . treeCursorCurrentL
       in case f of
            FilterHasTag t -> t `elem` entryTags cur
            FilterTodoState mts -> Just mts == entryState cur
            FilterFile t -> fromRelFile t `isInfixOf` fromAbsFile (resolveRootedPath rp)
            FilterProperty pf ->
              case pf of
                ExactProperty pn pv ->
                  case M.lookup pn $ entryProperties cur of
                    Nothing -> False
                    Just pv' -> pv == pv'
                HasProperty pn -> isJust $ M.lookup pn $ entryProperties cur
            FilterLevel l -> l == level fc
            FilterParent f' -> maybe False (go f') parent_
            FilterAncestor f' -> maybe False (\fc_ -> go f' fc_ || go f fc_) parent_
            FilterChild f' -> any (go f') children_
            FilterLegacy f' -> any (\fc_ -> go f' fc_ || go f fc_) children_
            FilterNot f' -> not $ go f' fc
            FilterAnd f1 f2 -> go f1 fc && go f2 fc
            FilterOr f1 f2 -> go f1 fc || go f2 fc
    level :: ForestCursor a -> Word
    level fc = go' $ fc ^. forestCursorSelectedTreeL
      where
        go' tc =
          case tc ^. treeCursorAboveL of
            Nothing -> 0
            Just tc' -> 1 + goA' tc'
        goA' ta =
          case treeAboveAbove ta of
            Nothing -> 0
            Just ta' -> 1 + goA' ta'

type P = Parsec Void Text

parseFilter :: Text -> Maybe Filter
parseFilter = parseMaybe filterP

filterP :: P Filter
filterP =
  try filterHasTagP <|> try filterTodoStateP <|> try filterFileP <|> try filterLevelP <|>
  try filterPropertyP <|>
  try filterParentP <|>
  try filterAncestorP <|>
  try filterChildP <|>
  try filterLegacyP <|>
  try filterNotP <|>
  filterBinRelP

filterHasTagP :: P Filter
filterHasTagP = do
  void $ string' "tag:"
  s <- many (satisfy $ \c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c))
  either fail (pure . FilterHasTag) $ parseTag $ T.pack s

filterTodoStateP :: P Filter
filterTodoStateP = do
  void $ string' "state:"
  s <- many (satisfy $ \c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c))
  either fail (pure . FilterTodoState) $ parseTodoState $ T.pack s

filterFileP :: P Filter
filterFileP = do
  void $ string' "file:"
  s <- many (satisfy $ \c -> not (Char.isSpace c) && c /= ')')
  r <- either (fail . show) (pure . FilterFile) $ parseRelFile s
  case prettyValidate r of
    Left err -> fail err
    Right f -> pure f

filterLevelP :: P Filter
filterLevelP = do
  void $ string' "level:"
  w <- decimal
  pure $ FilterLevel w

filterPropertyP :: P Filter
filterPropertyP = do
  void $ string' "property:"
  pf <- propertyFilterP
  pure $ FilterProperty pf

filterParentP :: P Filter
filterParentP = do
  void $ string' "parent:"
  FilterParent <$> filterP

filterAncestorP :: P Filter
filterAncestorP = do
  void $ string' "ancestor:"
  FilterAncestor <$> filterP

filterChildP :: P Filter
filterChildP = do
  void $ string' "child:"
  FilterChild <$> filterP

filterLegacyP :: P Filter
filterLegacyP = do
  void $ string' "legacy:"
  FilterLegacy <$> filterP

filterNotP :: P Filter
filterNotP = do
  void $ string' "not:"
  FilterNot <$> filterP

filterBinRelP :: P Filter
filterBinRelP = do
  void $ char '('
  f <- try filterOrP <|> filterAndP
  void $ char ')'
  pure f

filterOrP :: P Filter
filterOrP = do
  f1 <- filterP
  void $ string' " or "
  f2 <- filterP
  pure $ FilterOr f1 f2

filterAndP :: P Filter
filterAndP = do
  f1 <- filterP
  void $ string' " and "
  f2 <- filterP
  pure $ FilterAnd f1 f2

propertyFilterP :: P PropertyFilter
propertyFilterP = do
  try exactPropertyP <|> hasPropertyP

exactPropertyP :: P PropertyFilter
exactPropertyP = do
  void $ string' "exact:"
  pn <- propertyNameP
  void $ string' ":"
  pv <- propertyValueP
  pure $ ExactProperty pn pv

hasPropertyP :: P PropertyFilter
hasPropertyP = do
  void $ string' "has:"
  pn <- propertyNameP
  pure $ HasProperty pn

propertyNameP :: P PropertyName
propertyNameP = do
  s <- many (satisfy $ \c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c))
  either fail pure $ parsePropertyName $ T.pack s

propertyValueP :: P PropertyValue
propertyValueP = do
  s <- many (satisfy $ \c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c))
  either fail pure $ parsePropertyValue $ T.pack s

renderFilter :: Filter -> Text
renderFilter f =
  case f of
    FilterHasTag t -> "tag:" <> tagText t
    FilterTodoState ts -> "state:" <> todoStateText ts
    FilterFile t -> "file:" <> T.pack (fromRelFile t)
    FilterLevel l -> "level:" <> T.pack (show l)
    FilterProperty fp -> "property:" <> renderPropertyFilter fp
    FilterParent f' -> "parent:" <> renderFilter f'
    FilterAncestor f' -> "ancestor:" <> renderFilter f'
    FilterChild f' -> "child:" <> renderFilter f'
    FilterLegacy f' -> "legacy:" <> renderFilter f'
    FilterNot f' -> "not:" <> renderFilter f'
    FilterOr f1 f2 -> T.concat ["(", renderFilter f1, " or ", renderFilter f2, ")"]
    FilterAnd f1 f2 -> T.concat ["(", renderFilter f1, " and ", renderFilter f2, ")"]

renderPropertyFilter :: PropertyFilter -> Text
renderPropertyFilter pf =
  case pf of
    ExactProperty pn pv -> "exact:" <> propertyNameText pn <> ":" <> propertyValueText pv
    HasProperty pn -> "has:" <> propertyNameText pn
