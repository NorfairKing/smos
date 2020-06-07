module Cursor.Tree.Promote where

import Prelude
import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..), maybe)
import Cursor.Tree.Base (currentTree, makeTreeCursorWithAbove)
import Cursor.Tree.Types (CForest(..), CTree(..), TreeAbove(..), TreeCursor, emptyCForest, openForest, unpackCForest)

-- | Promotes the current node to the level of its parent.
--
-- Example:
--
-- Before:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |  |  |- c
-- >  |  |- d <--
-- >  |  |  |- e
-- >  |  |- f
-- >  |     |- g
-- >  |- h
--
-- After:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |  |  |- c
-- >  |  |  |- e
-- >  |  |- f
-- >  |     |- g
-- >  |- d <--
-- >  |- h
treeCursorPromoteElem ::
  forall a b.
  (a -> b) -> (b -> a) -> TreeCursor a b -> PromoteElemResult (TreeCursor a b)
treeCursorPromoteElem f g tc = do
  TreeAbove ta <- maybe CannotPromoteTopElem pure $ tc.treeAbove
  -- We need to put the below under the above lefts at the end
  lefts <- case tc.treeBelow of
    EmptyCForest -> pure $ ta.treeAboveLefts
    _ -> case ta.treeAboveLefts of
      Nil -> NoSiblingsToAdoptChildren
      (CTree ct : ts) -> pure $ CTree { rootLabel: ct.rootLabel, subForest: openForest $ unpackCForest ct.subForest <> unpackCForest tc.treeBelow } : ts
  TreeAbove taa <- maybe NoGrandparentToPromoteElemUnder pure ta.treeAboveAbove
  pure
    $ makeTreeCursorWithAbove g (CTree { rootLabel: f tc.treeCurrent, subForest: emptyCForest })
    $ Just
    $ TreeAbove
        ( taa
            { treeAboveLefts =
              CTree { rootLabel: ta.treeAboveNode, subForest: openForest $ reverse lefts <> ta.treeAboveRights }
                : taa.treeAboveLefts
            }
        )

data PromoteElemResult a
  = CannotPromoteTopElem
  | NoGrandparentToPromoteElemUnder
  | NoSiblingsToAdoptChildren
  | PromotedElem a

derive instance functorPromoteElemResult :: Functor PromoteElemResult

instance applicativePromoteElemResult :: Applicative PromoteElemResult where
  pure = PromotedElem

instance applyPromoteElemResult :: Apply PromoteElemResult where
  apply CannotPromoteTopElem _ = CannotPromoteTopElem
  apply NoGrandparentToPromoteElemUnder _ = NoGrandparentToPromoteElemUnder
  apply NoSiblingsToAdoptChildren _ = NoSiblingsToAdoptChildren
  apply (PromotedElem f) (PromotedElem a) = PromotedElem $ f a
  apply (PromotedElem _) CannotPromoteTopElem = CannotPromoteTopElem
  apply (PromotedElem _) NoSiblingsToAdoptChildren = NoSiblingsToAdoptChildren
  apply (PromotedElem _) NoGrandparentToPromoteElemUnder = NoGrandparentToPromoteElemUnder

instance bindPromoteElemResult :: Bind PromoteElemResult where
  bind CannotPromoteTopElem _ = CannotPromoteTopElem
  bind NoGrandparentToPromoteElemUnder _ = NoGrandparentToPromoteElemUnder
  bind NoSiblingsToAdoptChildren _ = NoSiblingsToAdoptChildren
  bind (PromotedElem a) f = f a

instance monadPromoteElemResult :: Monad PromoteElemResult

dullPromoteElemResult :: forall a. PromoteElemResult a -> Maybe a
dullPromoteElemResult = case _ of
  CannotPromoteTopElem -> Nothing
  NoGrandparentToPromoteElemUnder -> Nothing
  NoSiblingsToAdoptChildren -> Nothing
  PromotedElem a -> Just a

-- | Promotes the current node to the level of its parent.
--
-- Example:
--
-- Before:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |  |  |- c
-- >  |  |- d <--
-- >  |  |  |- e
-- >  |  |- f
-- >  |     |- g
-- >  |- h
--
-- After:
--
-- >  p
-- >  |- a
-- >  |  |- b
-- >  |  |  |- c
-- >  |  |- f
-- >  |     |- g
-- >  |- d <--
-- >  |  |- e
-- >  |- h
treeCursorPromoteSubTree :: forall a b. (a -> b) -> (b -> a) -> TreeCursor a b -> PromoteResult (TreeCursor a b)
treeCursorPromoteSubTree f g tc = do
  TreeAbove ta <- maybe CannotPromoteTopNode pure tc.treeAbove
  TreeAbove taa <- maybe NoGrandparentToPromoteUnder pure ta.treeAboveAbove
  pure
    $ makeTreeCursorWithAbove g (currentTree f tc)
    $ Just
    $ TreeAbove
        ( taa
            { treeAboveLefts =
              CTree { rootLabel: ta.treeAboveNode, subForest: openForest $ reverse ta.treeAboveLefts <> ta.treeAboveRights }
                : taa.treeAboveLefts
            }
        )

data PromoteResult a
  = CannotPromoteTopNode
  | NoGrandparentToPromoteUnder
  | Promoted a

derive instance functorPromoteResult :: Functor PromoteResult

instance applicativePromoteResult :: Applicative PromoteResult where
  pure = Promoted

instance applyPromoteResult :: Apply PromoteResult where
  apply CannotPromoteTopNode _ = CannotPromoteTopNode
  apply NoGrandparentToPromoteUnder _ = NoGrandparentToPromoteUnder
  apply (Promoted f) (Promoted a) = Promoted $ f a
  apply (Promoted _) CannotPromoteTopNode = CannotPromoteTopNode
  apply (Promoted _) NoGrandparentToPromoteUnder = NoGrandparentToPromoteUnder

instance bindPromoteResult :: Bind PromoteResult where
  bind CannotPromoteTopNode _ = CannotPromoteTopNode
  bind NoGrandparentToPromoteUnder _ = NoGrandparentToPromoteUnder
  bind (Promoted a) f = f a

instance monadPromoteResult :: Monad PromoteResult

dullPromoteResult :: forall a. PromoteResult a -> Maybe a
dullPromoteResult = case _ of
  CannotPromoteTopNode -> Nothing
  NoGrandparentToPromoteUnder -> Nothing
  Promoted a -> Just a
