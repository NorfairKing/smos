module Cursor.Tree.Collapse where

import Prelude
import Data.Maybe (Maybe(..))
import Cursor.Tree.Types (CForest(..), TreeCursor, cTree, makeCTree, rebuildCTree)

treeCursorOpenCurrentForest :: forall a b. TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorOpenCurrentForest tc = case tc.treeBelow of
  EmptyCForest -> Nothing
  ClosedForest ts -> Just $ tc { treeBelow = OpenForest $ map makeCTree ts }
  OpenForest _ -> Nothing

treeCursorCloseCurrentForest :: forall a b. TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorCloseCurrentForest tc = case tc.treeBelow of
  EmptyCForest -> Nothing
  ClosedForest _ -> Nothing
  OpenForest ts -> Just $ tc { treeBelow = ClosedForest $ map rebuildCTree ts }

treeCursorToggleCurrentForest :: forall a b. TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorToggleCurrentForest tc = case tc.treeBelow of
  EmptyCForest -> Nothing
  ClosedForest ts -> Just $ tc { treeBelow = OpenForest $ map makeCTree ts }
  OpenForest ts -> Just $ tc { treeBelow = ClosedForest $ map rebuildCTree ts }

treeCursorOpenCurrentForestRecursively :: forall a b. TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorOpenCurrentForestRecursively tc = case tc.treeBelow of
  EmptyCForest -> Nothing
  ClosedForest ts -> Just $ tc { treeBelow = OpenForest $ map (cTree true) ts }
  OpenForest _ -> Nothing

treeCursorToggleCurrentForestRecursively :: forall a b. TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorToggleCurrentForestRecursively tc = case tc.treeBelow of
  EmptyCForest -> Nothing
  ClosedForest ts -> Just $ tc { treeBelow = OpenForest $ map (cTree true) ts }
  OpenForest ts -> Just $ tc { treeBelow = ClosedForest $ map rebuildCTree ts }
