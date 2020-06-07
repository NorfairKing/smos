module Cursor.Tree.Demote where

import Prelude
import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..))
import Cursor.Tree.Base (currentTree, makeTreeCursorWithAbove)
import Cursor.Tree.Types (CTree(..), TreeAbove(..), TreeCursor, emptyCForest, unpackCForest)

-- | Demotes the current node to the level of its children.
--
-- Example:
--
-- Before:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |- c <--
-- >  |  |- d
-- >  |- e
--
-- After:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |  |- c <--
-- >  |  |- d
-- >  |- e
treeCursorDemoteElem :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> DemoteResult (TreeCursor a b)
treeCursorDemoteElem f g tc = case tc.treeAbove of
  Nothing -> CannotDemoteTopNode
  Just (TreeAbove ta) -> case ta.treeAboveLefts of
    Nil -> NoSiblingsToDemoteUnder
    (CTree ct : ts) ->
      Demoted
        $ makeTreeCursorWithAbove g
            ( CTree
                { rootLabel: f tc.treeCurrent
                , subForest: emptyCForest
                }
            )
        $ Just
            ( TreeAbove
                { treeAboveLefts: reverse $ unpackCForest ct.subForest
                , treeAboveAbove: Just (TreeAbove ta { treeAboveLefts = ts })
                , treeAboveNode: ct.rootLabel
                , treeAboveRights: unpackCForest $ tc.treeBelow
                }
            )

-- | Demotes the current subtree to the level of its children.
--
-- Example:
--
-- Before:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |- c <--
-- >  |  |- d
-- >  |- e
--
-- After:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |  |- c <--
-- >  |     |- d
-- >  |- e
treeCursorDemoteSubTree :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> DemoteResult (TreeCursor a b)
treeCursorDemoteSubTree f g tc = case tc.treeAbove of
  Nothing -> CannotDemoteTopNode
  Just (TreeAbove ta) -> case ta.treeAboveLefts of
    Nil -> NoSiblingsToDemoteUnder
    (CTree ct : ts) ->
      Demoted
        $ makeTreeCursorWithAbove g (currentTree f tc)
        $ Just
        $ TreeAbove
            { treeAboveLefts: reverse $ unpackCForest ct.subForest
            , treeAboveAbove: Just (TreeAbove ta { treeAboveLefts = ts })
            , treeAboveNode: ct.rootLabel
            , treeAboveRights: Nil
            }

data DemoteResult a
  = CannotDemoteTopNode
  | NoSiblingsToDemoteUnder
  | Demoted a

derive instance functorDemoteResult :: Functor DemoteResult

dullDemoteResult :: forall a. DemoteResult a -> Maybe a
dullDemoteResult = case _ of
  CannotDemoteTopNode -> Nothing
  NoSiblingsToDemoteUnder -> Nothing
  Demoted a -> Just a

-- | Demotes the current node to the level of its children, by adding two roots.
-- One for the current node and one for its children that are left behind.
--
-- Example:
--
-- Before:
--
-- >  p
-- >  |- a <--
-- >     |- b
--
-- After:
--
-- >  p
-- >  |- <given element 1>
-- >  |  |- a <--
-- >  |- <given element 2>
-- >  |  |- b
treeCursorDemoteElemUnder :: forall a b. b -> b -> TreeCursor a b -> Maybe (TreeCursor a b)
treeCursorDemoteElemUnder b1 b2 tc = case tc.treeAbove of
  Nothing -> Nothing
  Just (TreeAbove ta) ->
    let
      ta' :: TreeAbove b
      ta' =
        TreeAbove
          ( ta
              { treeAboveRights =
                CTree
                  { rootLabel: b2
                  , subForest: tc.treeBelow
                  }
                  : ta.treeAboveRights
              }
          )
    in
      Just
        $ { treeCurrent: tc.treeCurrent
          , treeAbove:
            Just
              $ TreeAbove
                  { treeAboveLefts: Nil
                  , treeAboveAbove: Just ta'
                  , treeAboveNode: b1
                  , treeAboveRights: Nil
                  }
          , treeBelow: emptyCForest
          }

-- | Demotes the current subtree to the level of its children, by adding a root.
--
-- Example:
--
-- Before:
--
-- >  a <--
-- >  |- b
--
-- After:
--
-- >  <given element>
-- >  |- a <--
-- >     |- b
treeCursorDemoteSubTreeUnder :: forall a b. b -> TreeCursor a b -> TreeCursor a b
treeCursorDemoteSubTreeUnder b tc =
  tc
    { treeAbove =
      Just
        $ TreeAbove
            { treeAboveLefts: Nil
            , treeAboveAbove: tc.treeAbove
            , treeAboveNode: b
            , treeAboveRights: Nil
            }
    }
