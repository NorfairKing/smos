module Cursor.Tree.Movement where

import Prelude
import Data.List
import Data.Maybe
import Control.Alternative
import Data.Tuple
import Cursor.Tree.Base
import Cursor.Tree.Types
import Data.List.NonEmpty as NE

treeCursorSelection :: forall a b. TreeCursor a b -> TreeCursorSelection
treeCursorSelection tc = wrap tc.treeAbove SelectNode
  where
  wrap :: forall a b. Maybe (TreeAbove a) -> TreeCursorSelection -> TreeCursorSelection
  wrap mta ts = case mta of
    Nothing -> ts
    Just (TreeAbove ta) -> wrap ta.treeAboveAbove (SelectChild (length ta.treeAboveLefts) ts)

treeCursorSelect ::
  forall a b.
  (a -> b) -> (b -> a) -> TreeCursorSelection -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelect f g sel = makeTreeCursorWithSelection f g sel <<< rebuildTreeCursor f

treeCursorSelectPrev :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectPrev f g tc =
  treeCursorSelectAbovePrev f g tc <|> treeCursorSelectPrevOnSameLevel f g tc
    <|> treeCursorSelectAbove f g tc

treeCursorSelectNext :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectNext f g tc =
  treeCursorSelectBelowAtStart f g tc <|> treeCursorSelectNextOnSameLevel f g tc
    <|> treeCursorSelectAboveNext f g tc

treeCursorSelectFirst :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> TreeCursor a b
treeCursorSelectFirst f g tc = maybe tc (treeCursorSelectFirst f g) $ treeCursorSelectPrev f g tc

treeCursorSelectLast :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> TreeCursor a b
treeCursorSelectLast f g tc = maybe tc (treeCursorSelectLast f g) $ treeCursorSelectNext f g tc

treeCursorSelectAbove :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectAbove f g tc = case tc.treeAbove of
  Nothing -> Nothing
  Just (TreeAbove ta) ->
    let
      newForest = reverse ta.treeAboveLefts <> (singleton (currentTree f tc)) <> ta.treeAboveRights

      newTree = CTree { rootLabel: ta.treeAboveNode, subForest: openForest newForest }
    in
      Just $ makeTreeCursorWithAbove g newTree ta.treeAboveAbove

treeCursorSelectBelowAtPos ::
  forall a b.
  (a -> b) -> (b -> a) -> Int -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectBelowAtPos f g pos tc = case tc.treeBelow of
  EmptyCForest -> Nothing
  ClosedForest _ -> Nothing
  OpenForest ts -> case splitAt pos $ NE.toList ts of
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

treeCursorSelectBelowAtStart :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectBelowAtStart f g = treeCursorSelectBelowAtPos f g 0

treeCursorSelectBelowAtEnd :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectBelowAtEnd f g tc = case tc.treeBelow of
  EmptyCForest -> Nothing
  ClosedForest _ -> Nothing
  OpenForest ts -> treeCursorSelectBelowAtPos f g (NE.length ts - 1) tc

treeCursorSelectBelowAtStartRecursively ::
  forall a b.
  (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectBelowAtStartRecursively f g tc = go <$> treeCursorSelectBelowAtStart f g tc
  where
  go c = maybe c go $ treeCursorSelectBelowAtStart f g c

treeCursorSelectBelowAtEndRecursively ::
  forall a b.
  (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectBelowAtEndRecursively f g tc = go <$> treeCursorSelectBelowAtEnd f g tc
  where
  go c = maybe c go $ treeCursorSelectBelowAtEnd f g c

treeCursorSelectPrevOnSameLevel :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectPrevOnSameLevel f g tc = do
  TreeAbove ta <- tc.treeAbove
  case ta.treeAboveLefts of
    Nil -> Nothing
    tree : xs -> Just (makeTreeCursorWithAbove g tree (Just (TreeAbove (ta { treeAboveLefts = xs, treeAboveRights = currentTree f tc : ta.treeAboveRights }))))

treeCursorSelectNextOnSameLevel :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectNextOnSameLevel f g tc = do
  TreeAbove ta <- tc.treeAbove
  case ta.treeAboveRights of
    Nil -> Nothing
    tree : xs -> Just (makeTreeCursorWithAbove g tree (Just (TreeAbove (ta { treeAboveLefts = currentTree f tc : ta.treeAboveLefts, treeAboveRights = xs }))))

treeCursorSelectFirstOnSameLevel :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> TreeCursor a b
treeCursorSelectFirstOnSameLevel f g tc = case treeCursorSelectPrevOnSameLevel f g tc of
  Nothing -> tc
  Just tc' -> treeCursorSelectFirstOnSameLevel f g tc'

treeCursorSelectLastOnSameLevel :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> TreeCursor a b
treeCursorSelectLastOnSameLevel f g tc = case treeCursorSelectNextOnSameLevel f g tc of
  Nothing -> tc
  Just tc' -> treeCursorSelectLastOnSameLevel f g tc'

-- | Go back and down as far as necessary to find a previous element on a level below
treeCursorSelectAbovePrev :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectAbovePrev f g = treeCursorSelectPrevOnSameLevel f g >=> treeCursorSelectBelowAtEndRecursively f g

-- | Go up as far as necessary to find a next element on a level above and forward
--
-- Note: This will fail if there is a next node on the same level or any node below the current node
treeCursorSelectAboveNext :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorSelectAboveNext f g tc = case treeCursorSelectNextOnSameLevel f g tc of
  Just _ -> Nothing
  Nothing -> case tc.treeBelow of
    EmptyCForest -> go tc
    ClosedForest _ -> go tc
    OpenForest ts -> Nothing
  where
  go tc_ = do
    tc' <- treeCursorSelectAbove f g tc_
    case treeCursorSelectNextOnSameLevel f g tc' of
      Nothing -> go tc'
      Just tc'' -> pure tc''
