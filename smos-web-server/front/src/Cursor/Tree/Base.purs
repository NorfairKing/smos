module Cursor.Tree.Base where

import Prelude
import Data.List (List(..), concat, drop, fromFoldable, reverse, singleton, take, (:))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Cursor.Tree.Types (CForest, CTree(..), TreeAbove(..), TreeCursor, TreeCursorSelection(..), emptyCForest, openForest, unpackCForest)

singletonTreeCursor :: forall a b. a -> TreeCursor a b
singletonTreeCursor v = { treeAbove: Nothing, treeCurrent: v, treeBelow: emptyCForest }

makeTreeCursor :: forall a b. (b -> a) -> CTree b -> TreeCursor a b
makeTreeCursor g (CTree cn) = { treeAbove: Nothing, treeCurrent: g cn.rootLabel, treeBelow: cn.subForest }

makeTreeCursorWithSelection ::
  forall a b.
  (a -> b) -> (b -> a) -> TreeCursorSelection -> CTree b -> Maybe (TreeCursor a b)
makeTreeCursorWithSelection f g sel = walkDown sel <<< makeTreeCursor g
  where
  walkDown SelectNode tc = pure tc

  walkDown (SelectChild i s) tc =
    (walkDown s =<< _)
      ( case splitAt i (unpackCForest tc.treeBelow) of
          Tuple _ Nil -> Nothing
          Tuple lefts (current : rights) ->
            Just
              ( makeTreeCursorWithAbove g current
                  ( Just
                      ( TreeAbove
                          { treeAboveLefts: reverse lefts
                          , treeAboveAbove: tc.treeAbove
                          , treeAboveNode: f tc.treeCurrent
                          , treeAboveRights: rights
                          }
                      )
                  )
              )
      )

rebuildTreeCursor :: forall a b. (a -> b) -> TreeCursor a b -> CTree b
rebuildTreeCursor f tc = wrapAbove tc.treeAbove (CTree { rootLabel: f tc.treeCurrent, subForest: tc.treeBelow })
  where
  wrapAbove mta t = case mta of
    Nothing -> t
    Just (TreeAbove ta) ->
      wrapAbove ta.treeAboveAbove
        $ CTree
            { rootLabel: ta.treeAboveNode
            , subForest:
              openForest
                $ concat
                $ fromFoldable
                    [ reverse ta.treeAboveLefts
                    , singleton t
                    , ta.treeAboveRights
                    ]
            }

mapTreeCursor :: forall a b c d. (a -> c) -> (b -> d) -> TreeCursor a b -> TreeCursor c d
mapTreeCursor f g tc = { treeAbove: map g <$> tc.treeAbove, treeCurrent: f tc.treeCurrent, treeBelow: map g tc.treeBelow }

currentTree :: forall a b. (a -> b) -> TreeCursor a b -> CTree b
currentTree f tc = CTree { rootLabel: f tc.treeCurrent, subForest: tc.treeBelow }

makeTreeCursorWithAbove :: forall a b. (b -> a) -> CTree b -> Maybe (TreeAbove b) -> TreeCursor a b
makeTreeCursorWithAbove g (CTree cn) mta = { treeAbove: mta, treeCurrent: g cn.rootLabel, treeBelow: cn.subForest }

foldTreeCursor ::
  forall a b c.
  (List (CTree b) -> b -> List (CTree b) -> c -> c) ->
  (a -> CForest b -> c) ->
  TreeCursor a b ->
  c
foldTreeCursor wrapFunc currentFunc tc = wrapAbove tc.treeAbove $ currentFunc tc.treeCurrent tc.treeBelow
  where
  wrapAbove :: Maybe (TreeAbove b) -> c -> c
  wrapAbove Nothing = identity

  wrapAbove (Just ta) = goAbove ta

  goAbove :: TreeAbove b -> c -> c
  goAbove (TreeAbove ta) = wrapAbove ta.treeAboveAbove <<< wrapFunc (reverse ta.treeAboveLefts) ta.treeAboveNode ta.treeAboveRights

splitAt :: forall a. Int -> List a -> Tuple (List a) (List a)
splitAt i l = Tuple (take i l) (drop i l)
