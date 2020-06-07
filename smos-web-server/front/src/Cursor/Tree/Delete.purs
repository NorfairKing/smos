module Cursor.Tree.Delete where

import Prelude
import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NE
import Data.NonEmpty ((:|))
import Control.Alternative ((<|>))
import Cursor.Tree.Base (makeTreeCursorWithAbove)
import Cursor.Tree.Types (CForest(..), CTree(..), Tree(..), TreeAbove(..), TreeCursor, closedForest, makeCTree, openForest, rebuildCTree, unpackCForest)
import Cursor.Types (DeleteOrUpdate(..), joinDeletes, joinDeletes3)

treeCursorDeleteSubTreeAndSelectPrevious ::
  forall a b.
  (b -> a) -> TreeCursor a b -> Maybe (DeleteOrUpdate (TreeCursor a b))
treeCursorDeleteSubTreeAndSelectPrevious g tc = case tc.treeAbove of
  Nothing -> Just Deleted
  Just (TreeAbove ta) -> case ta.treeAboveLefts of
    Nil -> Nothing
    tree : xs -> Just <<< Updated <<< makeTreeCursorWithAbove g tree $ Just (TreeAbove (ta { treeAboveLefts = xs }))

treeCursorDeleteSubTreeAndSelectNext ::
  forall a b.
  (b -> a) -> TreeCursor a b -> Maybe (DeleteOrUpdate (TreeCursor a b))
treeCursorDeleteSubTreeAndSelectNext g tc = case tc.treeAbove of
  Nothing -> Just Deleted
  Just (TreeAbove ta) -> case ta.treeAboveRights of
    Nil -> Nothing
    tree : xs -> Just <<< Updated <<< makeTreeCursorWithAbove g tree $ Just (TreeAbove (ta { treeAboveRights = xs }))

treeCursorDeleteSubTreeAndSelectAbove ::
  forall a b.
  (b -> a) -> TreeCursor a b -> DeleteOrUpdate (TreeCursor a b)
treeCursorDeleteSubTreeAndSelectAbove g tc = case tc.treeAbove of
  Nothing -> Deleted
  Just (TreeAbove ta) ->
    Updated
      { treeAbove: ta.treeAboveAbove
      , treeCurrent: g ta.treeAboveNode
      , treeBelow: openForest $ reverse ta.treeAboveLefts <> ta.treeAboveRights
      }

treeCursorRemoveSubTree :: forall a b. (b -> a) -> TreeCursor a b -> DeleteOrUpdate (TreeCursor a b)
treeCursorRemoveSubTree g tc =
  joinDeletes
    (treeCursorDeleteSubTreeAndSelectPrevious g tc)
    (treeCursorDeleteSubTreeAndSelectNext g tc)
    <|> treeCursorDeleteSubTreeAndSelectAbove g tc

treeCursorDeleteSubTree :: forall a b. (b -> a) -> TreeCursor a b -> DeleteOrUpdate (TreeCursor a b)
treeCursorDeleteSubTree g tc =
  joinDeletes
    (treeCursorDeleteSubTreeAndSelectNext g tc)
    (treeCursorDeleteSubTreeAndSelectPrevious g tc)
    <|> treeCursorDeleteSubTreeAndSelectAbove g tc

treeCursorDeleteElemAndSelectPrevious ::
  forall a b.
  (b -> a) -> TreeCursor a b -> Maybe (DeleteOrUpdate (TreeCursor a b))
treeCursorDeleteElemAndSelectPrevious g tc = case tc.treeAbove of
  Nothing -> case tc.treeBelow of
    EmptyCForest -> Just Deleted
    _ -> Nothing
  Just (TreeAbove ta) -> case ta.treeAboveLefts of
    Nil -> Nothing
    tree : xs ->
      Just <<< Updated <<< makeTreeCursorWithAbove g tree
        $ Just
            ( TreeAbove
                ( ta
                    { treeAboveLefts = xs
                    , treeAboveRights = unpackCForest tc.treeBelow <> ta.treeAboveRights
                    }
                )
            )

treeCursorDeleteElemAndSelectNext ::
  forall a b.
  (b -> a) -> TreeCursor a b -> Maybe (DeleteOrUpdate (TreeCursor a b))
treeCursorDeleteElemAndSelectNext g tc = case tc.treeBelow of
  EmptyCForest -> case tc.treeAbove of
    Nothing -> Just Deleted
    Just (TreeAbove ta) -> case ta.treeAboveRights of
      Nil -> Nothing
      tree : xs -> Just <<< Updated <<< makeTreeCursorWithAbove g tree $ Just (TreeAbove (ta { treeAboveRights = xs }))
  ClosedForest ts -> case tc.treeAbove of
    Nothing -> case ts of
      (NonEmptyList (Tree t :| xs)) ->
        let
          ct = CTree { rootLabel: t.rootLabel, subForest: closedForest $ t.subForest <> xs }
        in
          Just <<< Updated $ makeTreeCursorWithAbove g ct tc.treeAbove
    Just (TreeAbove ta) -> case ta.treeAboveRights of
      Nil -> Nothing
      tree : xs ->
        Just <<< Updated <<< makeTreeCursorWithAbove g tree
          $ Just
              ( TreeAbove
                  ( ta
                      { treeAboveLefts = map makeCTree (reverse $ NE.toList ts) <> ta.treeAboveLefts
                      , treeAboveRights = xs
                      }
                  )
              )
  OpenForest (NonEmptyList (CTree t :| xs)) ->
    let
      ct =
        CTree
          { rootLabel: t.rootLabel
          , subForest:
            case t.subForest of
              EmptyCForest -> openForest xs
              OpenForest ts_ -> openForest $ NE.toList ts_ <> xs
              ClosedForest ts_ -> closedForest $ NE.toList ts_ <> map rebuildCTree xs
          }
    in
      Just <<< Updated $ makeTreeCursorWithAbove g ct tc.treeAbove

treeCursorDeleteElemAndSelectAbove ::
  forall a b.
  (b -> a) -> TreeCursor a b -> Maybe (DeleteOrUpdate (TreeCursor a b))
treeCursorDeleteElemAndSelectAbove g tc = case tc.treeAbove of
  Nothing -> case tc.treeBelow of
    EmptyCForest -> Just Deleted
    _ -> Nothing
  Just (TreeAbove ta) ->
    Just
      $ Updated
          { treeAbove: ta.treeAboveAbove
          , treeCurrent: g ta.treeAboveNode
          , treeBelow:
            openForest $ reverse ta.treeAboveLefts <> unpackCForest tc.treeBelow <> ta.treeAboveRights
          }

treeCursorRemoveElem :: forall a b. (b -> a) -> TreeCursor a b -> DeleteOrUpdate (TreeCursor a b)
treeCursorRemoveElem g tc =
  joinDeletes3
    (treeCursorDeleteElemAndSelectPrevious g tc)
    (treeCursorDeleteElemAndSelectNext g tc)
    (treeCursorDeleteElemAndSelectAbove g tc)

treeCursorDeleteElem :: forall a b. (b -> a) -> TreeCursor a b -> DeleteOrUpdate (TreeCursor a b)
treeCursorDeleteElem g tc =
  joinDeletes3
    (treeCursorDeleteElemAndSelectNext g tc)
    (treeCursorDeleteElemAndSelectPrevious g tc)
    (treeCursorDeleteElemAndSelectAbove g tc)
