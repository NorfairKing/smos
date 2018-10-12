{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Query where

import GHC.Generics (Generic)

import Data.Function
import Data.Validity

import Lens.Micro

import Cursor.Simple.Forest
import Cursor.Simple.Tree
import Smos.Data

data Filter
    = FilterHasTag Tag
    | FilterTodoState (Maybe TodoState)
    | FilterParent Filter
    | FilterAncestor Filter
    | FilterNot Filter
    | FilterAnd Filter
                Filter
    | FilterOr Filter
               Filter
    deriving (Show, Eq, Generic)

instance Validity Filter

filterPredicate :: Filter -> ForestCursor Entry -> Bool
filterPredicate f fc =
    case f of
        FilterHasTag t -> t `elem` entryTags cur
        FilterTodoState mts -> mts == entryState cur
        FilterParent f' -> maybe False (filterPredicate f') parent
        FilterAncestor f' ->
            maybe
                False
                (\fc_ -> filterPredicate f' fc_ || filterPredicate f fc_)
                parent
        FilterNot f' -> not $ filterPredicate f' fc
        FilterAnd f1 f2 -> filterPredicate f1 fc && filterPredicate f2 fc
        FilterOr f1 f2 -> filterPredicate f1 fc || filterPredicate f2 fc
  where
    parent :: Maybe (ForestCursor Entry)
    parent = fc & forestCursorSelectedTreeL treeCursorSelectAbove
    cur :: Entry
    cur = fc ^. forestCursorSelectedTreeL . treeCursorCurrentL
