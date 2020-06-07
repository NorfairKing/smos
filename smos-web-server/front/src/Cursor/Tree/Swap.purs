module Cursor.Tree.Swap where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Prelude
import Cursor.Tree.Types (TreeAbove(..), TreeCursor)

-- | Swaps the current node with the previous node on the same level
--
-- Example:
--
-- Before:
--
-- > p
-- > |- a
-- > |- b <--
--
-- After:
--
-- > p
-- > |- b <--
-- > |- a
treeCursorSwapPrev :: forall a b. TreeCursor a b -> SwapResult (TreeCursor a b)
treeCursorSwapPrev tc = case tc.treeAbove of
  Nothing -> SwapperIsTopNode
  Just (TreeAbove ta) -> case ta.treeAboveLefts of
    Nil -> NoSiblingsToSwapWith
    (t : ts) ->
      Swapped
        $ tc
            { treeAbove =
              Just
                ( TreeAbove
                    ( ta
                        { treeAboveLefts = ts
                        , treeAboveRights = t : ta.treeAboveRights
                        }
                    )
                )
            }

-- | Swaps the current node with the next node on the same level
--
-- Example:
--
-- Before:
--
-- > p
-- > |- a <--
-- > |- b
--
-- After:
--
-- > p
-- > |- b
-- > |- a <--
treeCursorSwapNext :: forall a b. TreeCursor a b -> SwapResult (TreeCursor a b)
treeCursorSwapNext tc = case tc.treeAbove of
  Nothing -> SwapperIsTopNode
  Just (TreeAbove ta) -> case ta.treeAboveRights of
    Nil -> NoSiblingsToSwapWith
    (t : ts) ->
      Swapped
        $ tc
            { treeAbove =
              Just
                ( TreeAbove
                    ( ta
                        { treeAboveLefts = t : ta.treeAboveLefts
                        , treeAboveRights = ts
                        }
                    )
                )
            }

data SwapResult a
  = SwapperIsTopNode
  | NoSiblingsToSwapWith
  | Swapped a
