{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Draw.Cursor
    ( drawVerticalMapCursor
    , drawMapCursor
    , drawVerticalForestCursor
    , drawForestCursor
    , drawTreeCursor
    , drawVerticalNonEmptyCursor
    , drawNonEmptyCursor
    ) where

import Control.Monad

import Brick.Types as B
import Brick.Widgets.Core as B

import Cursor.Forest hiding (drawForestCursor)
import Cursor.List.NonEmpty
import Cursor.Map
import Cursor.Tree hiding (drawTreeCursor)

drawVerticalMapCursor ::
       Monad m
    => (k -> v -> m (Widget n))
    -> (KeyValueCursor kc vc k v -> m (Widget n))
    -> (k -> v -> m (Widget n))
    -> MapCursor kc vc k v
    -> m (Widget n)
drawVerticalMapCursor prevFunc curFunc nextFunc =
    drawMapCursor
        prevFunc
        curFunc
        nextFunc
        B.vBox
        B.vBox
        (\a b c -> a <=> b <=> c)

drawMapCursor ::
       Monad m
    => (k -> v -> m (Widget n))
    -> (KeyValueCursor kc vc k v -> m (Widget n))
    -> (k -> v -> m (Widget n))
    -> ([Widget n] -> Widget n)
    -> ([Widget n] -> Widget n)
    -> (Widget n -> Widget n -> Widget n -> Widget n)
    -> MapCursor kc vc k v
    -> m (Widget n)
drawMapCursor prevFunc curFunc nextFunc prevCombFunc nextCombFunc combFunc =
    drawNonEmptyCursor
        (uncurry prevFunc)
        curFunc
        (uncurry nextFunc)
        prevCombFunc
        nextCombFunc
        combFunc .
    mapCursorList

drawVerticalForestCursor ::
       Monad m
    => (CTree b -> m (Widget n))
    -> (TreeCursor a b -> m (Widget n))
    -> (CTree b -> m (Widget n))
    -> ForestCursor a b
    -> m (Widget n)
drawVerticalForestCursor prevFunc curFunc nextFunc fc =
    drawVerticalNonEmptyCursor prevFunc curFunc nextFunc $
    forestCursorListCursor fc

drawForestCursor ::
       Monad m
    => (CTree b -> m (Widget n))
    -> (TreeCursor a b -> m (Widget n))
    -> (CTree b -> m (Widget n))
    -> ([Widget n] -> Widget n)
    -> ([Widget n] -> Widget n)
    -> (Widget n -> Widget n -> Widget n -> Widget n)
    -> ForestCursor a b
    -> m (Widget n)
drawForestCursor prevFunc curFunc nextFunc prevCombFunc nextCombFunc combFunc fc =
    drawNonEmptyCursor
        prevFunc
        curFunc
        nextFunc
        prevCombFunc
        nextCombFunc
        combFunc $
    forestCursorListCursor fc

drawTreeCursor ::
       forall a b n m. Monad m
    => ([CTree b] -> b -> [CTree b] -> Widget n -> m (Widget n))
    -> (a -> CForest b -> m (Widget n))
    -> TreeCursor a b
    -> m (Widget n)
drawTreeCursor wrapAboveFunc currentFunc TreeCursor {..} =
    currentFunc treeCurrent treeBelow >>= wrapAbove treeAbove
  where
    wrapAbove :: Maybe (TreeAbove b) -> Widget n -> m (Widget n)
    wrapAbove Nothing = pure
    wrapAbove (Just ta) = goAbove ta
    goAbove :: TreeAbove b -> Widget n -> m (Widget n)
    goAbove TreeAbove {..} =
        wrapAboveFunc (reverse treeAboveLefts) treeAboveNode treeAboveRights >=>
        wrapAbove treeAboveAbove

drawVerticalNonEmptyCursor ::
       Monad m
    => (b -> m (Widget n))
    -> (a -> m (Widget n))
    -> (b -> m (Widget n))
    -> NonEmptyCursor a b
    -> m (Widget n)
drawVerticalNonEmptyCursor prevFunc curFunc nextFunc =
    drawNonEmptyCursor
        prevFunc
        curFunc
        nextFunc
        B.vBox
        B.vBox
        (\a b c -> a <=> b <=> c)

drawNonEmptyCursor ::
       Monad m
    => (b -> m (Widget n))
    -> (a -> m (Widget n))
    -> (b -> m (Widget n))
    -> ([Widget n] -> Widget n)
    -> ([Widget n] -> Widget n)
    -> (Widget n -> Widget n -> Widget n -> Widget n)
    -> NonEmptyCursor a b
    -> m (Widget n)
drawNonEmptyCursor prevFunc curFunc nextFunc prevCombFunc nextCombFunc combFunc NonEmptyCursor {..} = do
    prev <- fmap prevCombFunc $ mapM prevFunc $ reverse nonEmptyCursorPrev
    cur <- curFunc nonEmptyCursorCurrent
    next <- fmap nextCombFunc $ mapM nextFunc nonEmptyCursorNext
    pure $ combFunc prev cur next
