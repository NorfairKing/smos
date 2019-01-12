{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Draw.Cursor
    ( drawVerticalMapCursor
    , drawMapCursor
    , drawVerticalForestCursor
    , drawForestCursorM
    , drawTreeCursorM
    , drawVerticalNonEmptyCursorTable
    , drawHorizontalNonEmptyCursor
    , drawVerticalNonEmptyCursor
    , drawNonEmptyCursor
    ) where

import Data.List

import Control.Monad
import Control.Monad.Identity

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
    drawMapCursor $ \ps c ns -> do
        ps' <- mapM (uncurry prevFunc) ps
        c' <- curFunc c
        ns' <- mapM (uncurry nextFunc) ns
        pure $ B.vBox $ ps' ++ [c'] ++ ns'

drawMapCursor ::
       Monad m
    => ([(k, v)] -> KeyValueCursor kc vc k v -> [(k, v)] -> m (Widget n))
    -> MapCursor kc vc k v
    -> m (Widget n)
drawMapCursor combFunc = drawNonEmptyCursorM combFunc . mapCursorList

drawVerticalForestCursor ::
       Monad m
    => (CTree b -> m (Widget n))
    -> (TreeCursor a b -> m (Widget n))
    -> (CTree b -> m (Widget n))
    -> ForestCursor a b
    -> m (Widget n)
drawVerticalForestCursor prevFunc curFunc nextFunc =
    drawVerticalNonEmptyCursorM prevFunc curFunc nextFunc .
    forestCursorListCursor

drawForestCursorM ::
       Monad m
    => ([CTree b] -> TreeCursor a b -> [CTree b] -> m (Widget n))
    -> ForestCursor a b
    -> m (Widget n)
drawForestCursorM combFunc =
    drawNonEmptyCursorM combFunc . forestCursorListCursor

drawTreeCursorM ::
       forall a b n m. Monad m
    => ([CTree b] -> b -> [CTree b] -> Widget n -> m (Widget n))
    -> (a -> CForest b -> m (Widget n))
    -> TreeCursor a b
    -> m (Widget n)
drawTreeCursorM wrapAboveFunc currentFunc TreeCursor {..} =
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
       (b -> Widget n)
    -> (a -> Widget n)
    -> (b -> Widget n)
    -> NonEmptyCursor a b
    -> Widget n
drawVerticalNonEmptyCursor prevFunc curFunc nextFunc =
    drawNonEmptyCursor
        (\ps c ns -> B.vBox $ map prevFunc ps ++ [curFunc c] ++ map nextFunc ns)

drawVerticalNonEmptyCursorM ::
       Monad m
    => (b -> m (Widget n))
    -> (a -> m (Widget n))
    -> (b -> m (Widget n))
    -> NonEmptyCursor a b
    -> m (Widget n)
drawVerticalNonEmptyCursorM prevFunc curFunc nextFunc =
    drawNonEmptyCursorM $ \ps c ns -> do
        ps' <- mapM prevFunc ps
        c' <- curFunc c
        ns' <- mapM nextFunc ns
        pure $ B.vBox $ ps' ++ [c'] ++ ns'

drawHorizontalNonEmptyCursor ::
       (b -> Widget n)
    -> (a -> Widget n)
    -> (b -> Widget n)
    -> NonEmptyCursor a b
    -> Widget n
drawHorizontalNonEmptyCursor prevFunc curFunc nextFunc =
    drawNonEmptyCursor
        (\ps c ns -> B.hBox $ map prevFunc ps ++ [curFunc c] ++ map nextFunc ns)

drawHorizontalNonEmptyCursorM ::
       Monad m
    => (b -> m (Widget n))
    -> (a -> m (Widget n))
    -> (b -> m (Widget n))
    -> NonEmptyCursor a b
    -> m (Widget n)
drawHorizontalNonEmptyCursorM prevFunc curFunc nextFunc =
    drawNonEmptyCursorM $ \ps c ns -> do
        ps' <- mapM prevFunc ps
        c' <- curFunc c
        ns' <- mapM nextFunc ns
        pure $ B.hBox $ ps' ++ [c'] ++ ns'

drawVerticalNonEmptyCursorTable ::
       (b -> [Widget n])
    -> (a -> [Widget n])
    -> (b -> [Widget n])
    -> NonEmptyCursor a b
    -> Widget n
drawVerticalNonEmptyCursorTable prevFunc curFunc nextFunc =
    drawNonEmptyCursor
        (\ps c ns ->
             drawTable $ map prevFunc ps ++ [curFunc c] ++ map nextFunc ns)

drawTable :: [[Widget n]] -> Widget n
drawTable = hBox . intersperse (str " ") . map vBox . transpose

drawNonEmptyCursor ::
       ([b] -> a -> [b] -> Widget n) -> NonEmptyCursor a b -> Widget n
drawNonEmptyCursor combFunc ne =
    runIdentity $ drawNonEmptyCursorM (\p c n -> Identity $ combFunc p c n) ne

drawNonEmptyCursorM ::
       Monad m
    => ([b] -> a -> [b] -> m (Widget n))
    -> NonEmptyCursor a b
    -> m (Widget n)
drawNonEmptyCursorM combFunc NonEmptyCursor {..} =
    combFunc
        (reverse nonEmptyCursorPrev)
        nonEmptyCursorCurrent
        nonEmptyCursorNext
