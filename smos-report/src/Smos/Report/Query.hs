{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Query where

import GHC.Generics (Generic)

import Data.Char as Char
import Data.Function
import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Data.Void
import Path

import Control.Monad

import Lens.Micro

import Text.Megaparsec
import Text.Megaparsec.Char

import Cursor.Simple.Forest
import Cursor.Simple.Tree

import Smos.Data

import Smos.Report.Path

data Filter
    = FilterHasTag Tag
    | FilterTodoState TodoState
    | FilterFile (Path Rel File) -- Substring of the filename
    | FilterParent Filter
    | FilterAncestor Filter
    | FilterNot Filter
    | FilterAnd Filter
                Filter
    | FilterOr Filter
               Filter
    deriving (Show, Eq, Generic)

instance Validity Filter where
    validate f =
        mconcat
            [ genericValidate f
            , case f of
                  FilterFile s ->
                      declare "The filenames are restricted" $
                      all (\c -> not (Char.isSpace c) && c /= ')') $
                      fromRelFile s
                  _ -> valid
            ]

filterPredicate :: Filter -> RootedPath -> ForestCursor Entry -> Bool
filterPredicate f_ rp = go f_
  where
    go f fc =
        let parent_ :: Maybe (ForestCursor Entry)
            parent_ = fc & forestCursorSelectedTreeL treeCursorSelectAbove
            cur :: Entry
            cur = fc ^. forestCursorSelectedTreeL . treeCursorCurrentL
         in case f of
                FilterHasTag t -> t `elem` entryTags cur
                FilterTodoState mts -> Just mts == entryState cur
                FilterFile t ->
                    fromRelFile t `isInfixOf` fromAbsFile (resolveRootedPath rp)
                FilterParent f' -> maybe False (go f') parent_
                FilterAncestor f' ->
                    maybe False (\fc_ -> go f' fc_ || go f fc_) parent_
                FilterNot f' -> not $ go f' fc
                FilterAnd f1 f2 -> go f1 fc && go f2 fc
                FilterOr f1 f2 -> go f1 fc || go f2 fc

type P = Parsec Void Text

parseFilter :: Text -> Maybe Filter
parseFilter = parseMaybe filterP

filterP :: P Filter
filterP =
    try filterHasTagP <|> try filterTodoStateP <|> try filterFileP <|>
    try filterParentP <|>
    try filterAncestorP <|>
    try filterNotP <|>
    filterBinRelP

filterHasTagP :: P Filter
filterHasTagP = do
    void $ string' "tag:"
    s <-
        many
            (satisfy $ \c ->
                 Char.isPrint c && not (Char.isSpace c) &&
                 not (Char.isPunctuation c))
    either fail (pure . FilterHasTag) $ parseTag $ T.pack s

filterTodoStateP :: P Filter
filterTodoStateP = do
    void $ string' "state:"
    s <-
        many
            (satisfy $ \c ->
                 Char.isPrint c && not (Char.isSpace c) &&
                 not (Char.isPunctuation c))
    either fail (pure . FilterTodoState) $ parseTodoState $ T.pack s

filterFileP :: P Filter
filterFileP = do
    void $ string' "file:"
    s <- many (satisfy $ \c -> not (Char.isSpace c) && c /= ')')
    either (fail . show) (pure . FilterFile) $ parseRelFile s

filterParentP :: P Filter
filterParentP = do
    void $ string' "parent:"
    FilterParent <$> filterP

filterAncestorP :: P Filter
filterAncestorP = do
    void $ string' "ancestor:"
    FilterAncestor <$> filterP

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

renderFilter :: Filter -> Text
renderFilter f =
    case f of
        FilterHasTag t -> "tag:" <> tagText t
        FilterTodoState ts -> "state:" <> todoStateText ts
        FilterFile t -> "file:" <> T.pack (fromRelFile t)
        FilterParent f' -> "parent:" <> renderFilter f'
        FilterAncestor f' -> "ancestor:" <> renderFilter f'
        FilterNot f' -> "not:" <> renderFilter f'
        FilterOr f1 f2 ->
            T.concat ["(", renderFilter f1, " or ", renderFilter f2, ")"]
        FilterAnd f1 f2 ->
            T.concat ["(", renderFilter f1, " and ", renderFilter f2, ")"]
