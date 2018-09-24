{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Draw.Cursor
    ( drawVerticalForestCursor
    , drawForestCursor
    , drawTreeCursor
    , drawVerticalNonEmptyCursor
    , drawNonEmptyCursor
    ) where

import Brick.Types as B
import Brick.Widgets.Core as B

import Cursor.Forest hiding (drawForestCursor)
import Cursor.List.NonEmpty
import Cursor.Tree hiding (drawTreeCursor)

drawVerticalForestCursor ::
       (CTree b -> Widget n)
    -> (TreeCursor a b -> Widget n)
    -> (CTree b -> Widget n)
    -> ForestCursor a b
    -> Widget n
drawVerticalForestCursor prevFunc curFunc nextFunc fc =
    drawVerticalNonEmptyCursor prevFunc curFunc nextFunc $
    forestCursorListCursor fc

drawForestCursor ::
       (CTree b -> Widget n)
    -> (TreeCursor a b -> Widget n)
    -> (CTree b -> Widget n)
    -> ([Widget n] -> Widget n)
    -> ([Widget n] -> Widget n)
    -> (Widget n -> Widget n -> Widget n -> Widget n)
    -> ForestCursor a b
    -> Widget n
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
       forall a b n.
       ([CTree b] -> b -> [CTree b] -> Widget n -> Widget n)
    -> (a -> CForest b -> Widget n)
    -> TreeCursor a b
    -> Widget n
drawTreeCursor wrapAboveFunc currentFunc TreeCursor {..} =
    wrapAbove treeAbove $ currentFunc treeCurrent treeBelow
  where
    wrapAbove :: Maybe (TreeAbove b) -> Widget n -> Widget n
    wrapAbove Nothing = id
    wrapAbove (Just ta) = goAbove ta
    goAbove :: TreeAbove b -> Widget n -> Widget n
    goAbove TreeAbove {..} =
        wrapAbove treeAboveAbove .
        wrapAboveFunc (reverse treeAboveLefts) treeAboveNode treeAboveRights

drawVerticalNonEmptyCursor ::
       (b -> Widget n)
    -> (a -> Widget n)
    -> (b -> Widget n)
    -> NonEmptyCursor a b
    -> Widget n
drawVerticalNonEmptyCursor prevFunc curFunc nextFunc =
    drawNonEmptyCursor
        prevFunc
        curFunc
        nextFunc
        B.vBox
        B.vBox
        (\a b c -> a <=> b <=> c)

drawNonEmptyCursor ::
       (b -> Widget n)
    -> (a -> Widget n)
    -> (b -> Widget n)
    -> ([Widget n] -> Widget n)
    -> ([Widget n] -> Widget n)
    -> (Widget n -> Widget n -> Widget n -> Widget n)
    -> NonEmptyCursor a b
    -> Widget n
drawNonEmptyCursor prevFunc curFunc nextFunc prevCombFunc nextCombFunc combFunc NonEmptyCursor {..} =
    let prev = prevCombFunc $ map prevFunc $ reverse nonEmptyCursorPrev
        cur = curFunc nonEmptyCursorCurrent
        next = nextCombFunc $ map nextFunc nonEmptyCursorNext
    in combFunc prev cur next
