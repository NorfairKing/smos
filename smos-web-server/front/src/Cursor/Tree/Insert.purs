module Cursor.Tree.Insert where

import Prelude
import Data.List
import Data.Maybe
import Cursor.Tree.Base
import Cursor.Tree.Types
import Data.List.NonEmpty as NE
import Data.Tuple

treeCursorInsert :: forall a b. Tree b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorInsert tree tc = do
  TreeAbove ta <- tc.treeAbove
  let
    newTreeAbove = TreeAbove ta { treeAboveLefts = makeCTree tree : ta.treeAboveLefts }
  pure tc { treeAbove = Just newTreeAbove }

treeCursorInsertAndSelect ::
  forall a b.
  (a -> b) -> (b -> a) -> Tree b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorInsertAndSelect f g tree tc = do
  TreeAbove ta <- tc.treeAbove
  let
    newTreeAbove = TreeAbove ta { treeAboveRights = currentTree f tc : ta.treeAboveRights }
  pure $ makeTreeCursorWithAbove g (makeCTree tree) $ Just newTreeAbove

treeCursorAppend :: forall a b. Tree b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorAppend tree tc = do
  TreeAbove ta <- tc.treeAbove
  let
    newTreeAbove = TreeAbove ta { treeAboveRights = makeCTree tree : ta.treeAboveRights }
  pure tc { treeAbove = Just newTreeAbove }

treeCursorAppendAndSelect ::
  forall a b.
  (a -> b) -> (b -> a) -> Tree b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorAppendAndSelect f g tree tc = do
  TreeAbove ta <- tc.treeAbove
  let
    newTreeAbove = TreeAbove ta { treeAboveLefts = currentTree f tc : ta.treeAboveLefts }
  pure $ makeTreeCursorWithAbove g (makeCTree tree) $ Just newTreeAbove

-- TODO make this fail if the position doesn't make sense
treeCursorAddChildAtPos :: forall a b. Int -> Tree b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildAtPos i t tc = case tc.treeBelow of
  EmptyCForest -> tc { treeBelow = openForest (singleton (makeCTree t)) }
  ClosedForest ts ->
    let
      Tuple before after = splitAt i (NE.toList ts)
    in
      tc { treeBelow = openForest (map makeCTree (before <> (singleton t) <> after)) }
  OpenForest ts ->
    let
      Tuple before after = splitAt i (NE.toList ts)
    in
      tc { treeBelow = openForest (before <> singleton (makeCTree t) <> after) }

treeCursorAddChildAtStart :: forall a b. Tree b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildAtStart t tc = case tc.treeBelow of
  EmptyCForest -> tc { treeBelow = openForest (singleton (makeCTree t)) }
  ClosedForest ts -> tc { treeBelow = OpenForest (map makeCTree (t `NE.cons` ts)) }
  OpenForest ts -> tc { treeBelow = OpenForest (makeCTree t `NE.cons` ts) }

treeCursorAddChildAtEnd :: forall a b. Tree b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildAtEnd t tc = case tc.treeBelow of
  EmptyCForest -> tc { treeBelow = openForest (singleton (makeCTree t)) }
  ClosedForest ts -> tc { treeBelow = openForest (map makeCTree (NE.toList (NE.snoc ts t))) }
  OpenForest ts -> tc { treeBelow = openForest (NE.toList (ts `NE.snoc` makeCTree t)) }

treeCursorAddChildAtPosAndSelect ::
  forall a b.
  (a -> b) -> (b -> a) -> Int -> Tree b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildAtPosAndSelect f g i (Tree t) tc =
  let
    Tuple before after = splitAt i (unpackCForest tc.treeBelow)
  in
    { treeAbove:
      Just
        ( TreeAbove
            { treeAboveLefts: before
            , treeAboveAbove: tc.treeAbove
            , treeAboveNode: f tc.treeCurrent
            , treeAboveRights: after
            }
        )
    , treeCurrent: g t.rootLabel
    , treeBelow: makeCForest t.subForest
    }

treeCursorAddChildAtStartAndSelect ::
  forall a b.
  (a -> b) -> (b -> a) -> Tree b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildAtStartAndSelect f g (Tree t) tc =
  { treeAbove:
    Just
      ( TreeAbove
          { treeAboveLefts: Nil
          , treeAboveAbove: tc.treeAbove
          , treeAboveNode: f tc.treeCurrent
          , treeAboveRights: unpackCForest tc.treeBelow
          }
      )
  , treeCurrent: g t.rootLabel
  , treeBelow: makeCForest t.subForest
  }

treeCursorAddChildAtEndAndSelect ::
  forall a b.
  (a -> b) -> (b -> a) -> Tree b -> TreeCursor a b -> TreeCursor a b
treeCursorAddChildAtEndAndSelect f g (Tree t) tc =
  { treeAbove:
    Just
      ( TreeAbove
          { treeAboveLefts: unpackCForest tc.treeBelow
          , treeAboveAbove: tc.treeAbove
          , treeAboveNode: f tc.treeCurrent
          , treeAboveRights: Nil
          }
      )
  , treeCurrent: g t.rootLabel
  , treeBelow: makeCForest t.subForest
  }
