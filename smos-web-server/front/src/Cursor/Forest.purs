module Cursor.Forest where

import Cursor.List.NonEmpty (NonEmptyCursor, foldNonEmptyCursor, makeNonEmptyCursor, mapNonEmptyCursor, nonEmptyCursorAppend, nonEmptyCursorAppendAndSelect, nonEmptyCursorDeleteElem, nonEmptyCursorDeleteElemAndSelectNext, nonEmptyCursorElemL, nonEmptyCursorInsert, nonEmptyCursorInsertAndSelect, nonEmptyCursorRemoveElem, nonEmptyCursorRemoveElemAndSelectPrev, nonEmptyCursorSelectFirst, nonEmptyCursorSelectIndex, nonEmptyCursorSelectLast, nonEmptyCursorSelectNext, nonEmptyCursorSelectPrev, nonEmptyCursorSelection, rebuildNonEmptyCursor, singletonNonEmptyCursor)
import Control.Alternative ((<|>))
import Prelude
import Cursor.Tree (PromoteElemResult(..), PromoteResult(..), SwapResult(..), makeTreeCursor, makeTreeCursorWithSelection, mapTreeCursor, rebuildTreeCursor, singletonTreeCursor, treeCursorAddChildAtEnd, treeCursorAddChildAtEndAndSelect, treeCursorAddChildAtPos, treeCursorAddChildAtPosAndSelect, treeCursorAddChildAtStart, treeCursorAddChildAtStartAndSelect, treeCursorAppend, treeCursorAppendAndSelect, treeCursorCloseCurrentForest, treeCursorDeleteElem, treeCursorDeleteElemAndSelectNext, treeCursorDeleteElemAndSelectPrevious, treeCursorDeleteSubTree, treeCursorDeleteSubTreeAndSelectNext, treeCursorDeleteSubTreeAndSelectPrevious, treeCursorInsert, treeCursorInsertAndSelect, treeCursorOpenCurrentForest, treeCursorOpenCurrentForestRecursively, treeCursorPromoteElem, treeCursorPromoteSubTree, treeCursorRemoveElem, treeCursorRemoveSubTree, treeCursorSwapNext, treeCursorSwapPrev, treeCursorToggleCurrentForest, treeCursorToggleCurrentForestRecursively)
import Cursor.Tree.Types (CForest(..), CTree(..), Tree(..), TreeAbove(..), TreeCursor, TreeCursorSelection(..), emptyCForest, lengthCForest, makeCTree, openForest, unpackCForest)
import Cursor.Tree.Demote (DemoteResult(..), treeCursorDemoteElem, treeCursorDemoteElemUnder, treeCursorDemoteSubTree, treeCursorDemoteSubTreeUnder)
import Cursor.Tree.Movement (PathToClickedEntry(..), treeCursorSelectAbove, treeCursorSelectBelowAtEnd, treeCursorSelectBelowAtEndRecursively, treeCursorSelectBelowAtPos, treeCursorSelectBelowAtStart, treeCursorSelectNext, treeCursorSelectNextOnSameLevel, treeCursorSelectPrev, treeCursorSelectPrevOnSameLevel)
import Cursor.Types (DeleteOrUpdate(..), focusPossibleDeleteOrUpdate, joinPossibleDeletes)
import Data.List.NonEmpty (NonEmptyList)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Lens (Lens, Lens', lens, traverseOf, (%~), (^.))

newtype ForestCursor a b
  = ForestCursor
  { forestCursorListCursor :: NonEmptyCursor (TreeCursor a b) (CTree b)
  }

singletonForestCursor :: forall a b. a -> ForestCursor a b
singletonForestCursor a =
  ForestCursor
    { forestCursorListCursor: singletonNonEmptyCursor (singletonTreeCursor a)
    }

makeForestCursor :: forall a b. (b -> a) -> NonEmptyList (CTree b) -> ForestCursor a b
makeForestCursor g nec = ForestCursor { forestCursorListCursor: makeNonEmptyCursor (makeTreeCursor g) nec }

rebuildForestCursor :: forall a b. (a -> b) -> ForestCursor a b -> NonEmptyList (CTree b)
rebuildForestCursor f (ForestCursor fc) = rebuildNonEmptyCursor (rebuildTreeCursor f) fc.forestCursorListCursor

mapForestCursor :: forall a b c d. (a -> c) -> (b -> d) -> ForestCursor a b -> ForestCursor c d
mapForestCursor f g = forestCursorListCursorL %~ mapNonEmptyCursor (mapTreeCursor f g) (map g)

forestCursorListCursorL ::
  forall a b c d.
  Lens (ForestCursor a b) (ForestCursor c d) (NonEmptyCursor (TreeCursor a b) (CTree b)) (NonEmptyCursor (TreeCursor c d) (CTree d))
forestCursorListCursorL = lens (\(ForestCursor fc) -> fc.forestCursorListCursor) $ \(ForestCursor fc) lc -> (ForestCursor fc { forestCursorListCursor = lc })

forestCursorSelectedTreeL :: forall a b. Lens' (ForestCursor a b) (TreeCursor a b)
forestCursorSelectedTreeL = forestCursorListCursorL <<< nonEmptyCursorElemL

forestCursorSelectPrevTreeCursor :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectPrevTreeCursor f g = (traverseOf forestCursorListCursorL) $ nonEmptyCursorSelectPrev (rebuildTreeCursor f) (makeTreeCursor g)

forestCursorSelectNextTreeCursor :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectNextTreeCursor f g = (traverseOf forestCursorListCursorL) $ nonEmptyCursorSelectNext (rebuildTreeCursor f) (makeTreeCursor g)

forestCursorSelectFirst :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> ForestCursor a b
forestCursorSelectFirst f g = forestCursorListCursorL %~ nonEmptyCursorSelectFirst (rebuildTreeCursor f) (makeTreeCursor g)

forestCursorSelectLast :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> ForestCursor a b
forestCursorSelectLast f g = forestCursorListCursorL %~ nonEmptyCursorSelectLast (rebuildTreeCursor f) (makeTreeCursor g)

forestCursorSelectNext :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectNext f g fc =
  (fc # (traverseOf forestCursorSelectedTreeL) (treeCursorSelectNext f g))
    <|> forestCursorSelectNextTreeCursor f g fc

forestCursorSelectPrev :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectPrev f g fc =
  (fc # (traverseOf forestCursorSelectedTreeL) (treeCursorSelectPrev f g))
    <|> ( forestCursorSelectPrevTreeCursor f g fc
          >>= (traverseOf forestCursorSelectedTreeL) (treeCursorSelectBelowAtEndRecursively f g)
      )
    <|> forestCursorSelectPrevTreeCursor f g fc

forestCursorSelectNextOnSameLevel ::
  forall a b.
  (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectNextOnSameLevel f g fc =
  (fc # (traverseOf forestCursorSelectedTreeL) (treeCursorSelectNextOnSameLevel f g))
    <|> forestCursorSelectNextTreeCursor f g fc

forestCursorSelectPrevOnSameLevel ::
  forall a b.
  (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectPrevOnSameLevel f g fc =
  (fc # (traverseOf forestCursorSelectedTreeL) (treeCursorSelectPrevOnSameLevel f g))
    <|> forestCursorSelectPrevTreeCursor f g fc

forestCursorSelectLastOnSameLevel :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> ForestCursor a b
forestCursorSelectLastOnSameLevel f g fc = case forestCursorSelectNextOnSameLevel f g fc of
  Nothing -> fc
  Just fc' -> forestCursorSelectLastOnSameLevel f g fc'

forestCursorSelectFirstOnSameLevel :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> ForestCursor a b
forestCursorSelectFirstOnSameLevel f g fc = case forestCursorSelectPrevOnSameLevel f g fc of
  Nothing -> fc
  Just fc' -> forestCursorSelectLastOnSameLevel f g fc'

forestCursorSelectFirstTreeCursor :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> ForestCursor a b
forestCursorSelectFirstTreeCursor f g fc = case forestCursorSelectPrev f g fc of
  Just fc' -> forestCursorSelectFirst f g fc'
  Nothing -> case forestCursorSelectPrev f g fc of
    Just fc' -> forestCursorSelectFirst f g fc'
    Nothing -> fc

forestCursorSelectLastTreeCursor :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> ForestCursor a b
forestCursorSelectLastTreeCursor f g fc = case forestCursorSelectNext f g fc of
  Just fc' -> forestCursorSelectLast f g fc'
  Nothing -> case forestCursorSelectNext f g fc of
    Just fc' -> forestCursorSelectLast f g fc'
    Nothing -> fc

forestCursorSelectAbove :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectAbove f g = (traverseOf forestCursorSelectedTreeL) $ treeCursorSelectAbove f g

forestCursorSelectBelowAtPos ::
  forall a b.
  (a -> b) -> (b -> a) -> Int -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectBelowAtPos f g i = (traverseOf forestCursorSelectedTreeL) $ treeCursorSelectBelowAtPos f g i

forestCursorSelectBelowAtStart ::
  forall a b.
  (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectBelowAtStart f g = (traverseOf forestCursorSelectedTreeL) $ treeCursorSelectBelowAtStart f g

forestCursorSelectBelowAtEnd :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectBelowAtEnd f g = (traverseOf forestCursorSelectedTreeL) $ treeCursorSelectBelowAtEnd f g

forestCursorSelection :: forall a b. ForestCursor a b -> Int
forestCursorSelection fc = nonEmptyCursorSelection $ fc ^. forestCursorListCursorL

forestCursorSelectIndex ::
  forall a b.
  (a -> b) -> (b -> a) -> Int -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSelectIndex f g i = (traverseOf forestCursorListCursorL) (nonEmptyCursorSelectIndex (rebuildTreeCursor f) (makeTreeCursor g) i)

forestCursorOpenCurrentForest :: forall a b. ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorOpenCurrentForest = (traverseOf forestCursorSelectedTreeL) treeCursorOpenCurrentForest

forestCursorCloseCurrentForest :: forall a b. ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorCloseCurrentForest = (traverseOf forestCursorSelectedTreeL) treeCursorCloseCurrentForest

forestCursorToggleCurrentForest :: forall a b. ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorToggleCurrentForest = (traverseOf forestCursorSelectedTreeL) treeCursorToggleCurrentForest

forestCursorOpenCurrentForestRecursively :: forall a b. ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorOpenCurrentForestRecursively = (traverseOf forestCursorSelectedTreeL) treeCursorOpenCurrentForestRecursively

forestCursorToggleCurrentForestRecursively :: forall a b. ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorToggleCurrentForestRecursively = (traverseOf forestCursorSelectedTreeL) treeCursorToggleCurrentForestRecursively

forestCursorInsertEntireTree :: forall a b. Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorInsertEntireTree t = forestCursorListCursorL %~ nonEmptyCursorInsert (makeCTree t)

forestCursorInsertAndSelectTreeCursor :: forall a b. (a -> b) -> TreeCursor a b -> ForestCursor a b -> ForestCursor a b
forestCursorInsertAndSelectTreeCursor f tc = forestCursorListCursorL %~ nonEmptyCursorInsertAndSelect (rebuildTreeCursor f) tc

forestCursorAppendEntireTree :: forall a b. Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAppendEntireTree t = forestCursorListCursorL %~ nonEmptyCursorAppend (makeCTree t)

forestCursorAppendAndSelectTreeCursor :: forall a b. (a -> b) -> TreeCursor a b -> ForestCursor a b -> ForestCursor a b
forestCursorAppendAndSelectTreeCursor f tc = forestCursorListCursorL %~ nonEmptyCursorAppendAndSelect (rebuildTreeCursor f) tc

forestCursorInsertTree :: forall a b. Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorInsertTree t fc =
  fromMaybe (forestCursorInsertEntireTree t fc)
    $ fc
    # (traverseOf forestCursorSelectedTreeL) (treeCursorInsert t)

forestCursorInsertAndSelectTree ::
  forall a b.
  (a -> b) -> (b -> a) -> Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorInsertAndSelectTree f g t fc =
  fromMaybe (forestCursorInsertAndSelectTreeCursor f (makeTreeCursor g $ makeCTree t) fc)
    $ fc
    # (traverseOf forestCursorSelectedTreeL) (treeCursorInsertAndSelect f g t)

forestCursorAppendTree :: forall a b. Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAppendTree t fc =
  fromMaybe (forestCursorAppendEntireTree t fc)
    $ fc
    # (traverseOf forestCursorSelectedTreeL) (treeCursorAppend t)

forestCursorAppendAndSelectTree ::
  forall a b.
  (a -> b) -> (b -> a) -> Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAppendAndSelectTree f g t fc =
  fromMaybe (forestCursorAppendAndSelectTreeCursor f (makeTreeCursor g $ makeCTree t) fc)
    $ fc
    # (traverseOf forestCursorSelectedTreeL) (treeCursorAppendAndSelect f g t)

forestCursorInsert :: forall a b. b -> ForestCursor a b -> ForestCursor a b
forestCursorInsert b = forestCursorInsertTree $ Tree { rootLabel: b, subForest: Nil }

forestCursorInsertAndSelect :: forall a b. (a -> b) -> (b -> a) -> b -> ForestCursor a b -> ForestCursor a b
forestCursorInsertAndSelect f g b = forestCursorInsertAndSelectTree f g $ Tree { rootLabel: b, subForest: Nil }

forestCursorAppend :: forall a b. b -> ForestCursor a b -> ForestCursor a b
forestCursorAppend b = forestCursorAppendTree $ Tree { rootLabel: b, subForest: Nil }

forestCursorAppendAndSelect :: forall a b. (a -> b) -> (b -> a) -> b -> ForestCursor a b -> ForestCursor a b
forestCursorAppendAndSelect f g b = forestCursorAppendAndSelectTree f g $ Tree { rootLabel: b, subForest: Nil }

forestCursorAddChildTreeToTreeAtPos :: forall a b. Int -> Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildTreeToTreeAtPos i t = forestCursorSelectedTreeL %~ treeCursorAddChildAtPos i t

forestCursorAddChildTreeToTreeAtStart :: forall a b. Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildTreeToTreeAtStart t = forestCursorSelectedTreeL %~ treeCursorAddChildAtStart t

forestCursorAddChildTreeToTreeAtEnd :: forall a b. Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildTreeToTreeAtEnd t fc = fc # forestCursorSelectedTreeL %~ treeCursorAddChildAtEnd t

forestCursorAddChildToTreeAtPos :: forall a b. Int -> b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildToTreeAtPos i b = forestCursorAddChildTreeToTreeAtPos i $ Tree { rootLabel: b, subForest: Nil }

forestCursorAddChildToTreeAtStart :: forall a b. b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildToTreeAtStart b = forestCursorAddChildTreeToTreeAtStart $ Tree { rootLabel: b, subForest: Nil }

forestCursorAddChildToTreeAtEnd :: forall a b. b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildToTreeAtEnd b = forestCursorAddChildTreeToTreeAtEnd $ Tree { rootLabel: b, subForest: Nil }

forestCursorAddChildTreeToTreeAtPosAndSelect ::
  forall a b.
  (a -> b) -> (b -> a) -> Int -> Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildTreeToTreeAtPosAndSelect f g i t = forestCursorSelectedTreeL %~ treeCursorAddChildAtPosAndSelect f g i t

forestCursorAddChildTreeToTreeAtStartAndSelect ::
  forall a b.
  (a -> b) -> (b -> a) -> Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildTreeToTreeAtStartAndSelect f g t = forestCursorSelectedTreeL %~ treeCursorAddChildAtStartAndSelect f g t

forestCursorAddChildTreeToTreeAtEndAndSelect ::
  forall a b.
  (a -> b) -> (b -> a) -> Tree b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildTreeToTreeAtEndAndSelect f g t fc = fc # forestCursorSelectedTreeL %~ treeCursorAddChildAtEndAndSelect f g t

forestCursorAddChildToTreeAtPosAndSelect ::
  forall a b.
  (a -> b) -> (b -> a) -> Int -> b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildToTreeAtPosAndSelect f g i b = forestCursorAddChildTreeToTreeAtPosAndSelect f g i $ Tree { rootLabel: b, subForest: Nil }

forestCursorAddChildToTreeAtStartAndSelect ::
  forall a b.
  (a -> b) -> (b -> a) -> b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildToTreeAtStartAndSelect f g b = forestCursorAddChildTreeToTreeAtStartAndSelect f g $ Tree { rootLabel: b, subForest: Nil }

forestCursorAddChildToTreeAtEndAndSelect ::
  forall a b.
  (a -> b) -> (b -> a) -> b -> ForestCursor a b -> ForestCursor a b
forestCursorAddChildToTreeAtEndAndSelect f g b = forestCursorAddChildTreeToTreeAtEndAndSelect f g $ Tree { rootLabel: b, subForest: Nil }

forestCursorRemoveElemAndSelectPrev ::
  forall a b.
  (b -> a) -> ForestCursor a b -> Maybe (DeleteOrUpdate (ForestCursor a b))
forestCursorRemoveElemAndSelectPrev g fc = case fc
    # focusPossibleDeleteOrUpdate
        forestCursorSelectedTreeL
        (treeCursorDeleteElemAndSelectPrevious g) of
  Just Deleted ->
    fc
      # focusPossibleDeleteOrUpdate
          forestCursorListCursorL
          (nonEmptyCursorRemoveElemAndSelectPrev (makeTreeCursor g))
  r -> r

forestCursorDeleteElemAndSelectNext ::
  forall a b.
  (b -> a) -> ForestCursor a b -> Maybe (DeleteOrUpdate (ForestCursor a b))
forestCursorDeleteElemAndSelectNext g fc = case fc
    # focusPossibleDeleteOrUpdate forestCursorSelectedTreeL (treeCursorDeleteElemAndSelectNext g) of
  Just Deleted ->
    fc
      # focusPossibleDeleteOrUpdate
          forestCursorListCursorL
          (nonEmptyCursorDeleteElemAndSelectNext (makeTreeCursor g))
  r -> r

forestCursorRemoveElem :: forall a b. (b -> a) -> ForestCursor a b -> DeleteOrUpdate (ForestCursor a b)
forestCursorRemoveElem g fc =
  (fc # (traverseOf forestCursorSelectedTreeL) (treeCursorRemoveElem g))
    <|> (fc # (traverseOf forestCursorListCursorL) (nonEmptyCursorRemoveElem (makeTreeCursor g)))

forestCursorDeleteElem :: forall a b. (b -> a) -> ForestCursor a b -> DeleteOrUpdate (ForestCursor a b)
forestCursorDeleteElem g fc =
  (fc # (traverseOf forestCursorSelectedTreeL) (treeCursorDeleteElem g))
    <|> (fc # (traverseOf forestCursorListCursorL) (nonEmptyCursorDeleteElem (makeTreeCursor g)))

forestCursorRemoveSubTreeAndSelectPrev ::
  forall a b.
  (b -> a) -> ForestCursor a b -> Maybe (DeleteOrUpdate (ForestCursor a b))
forestCursorRemoveSubTreeAndSelectPrev g fc =
  joinPossibleDeletes
    ( fc
        # focusPossibleDeleteOrUpdate
            forestCursorSelectedTreeL
            (treeCursorDeleteSubTreeAndSelectPrevious g)
    )
    ( fc
        # focusPossibleDeleteOrUpdate
            forestCursorListCursorL
            (nonEmptyCursorRemoveElemAndSelectPrev (makeTreeCursor g))
    )

forestCursorDeleteSubTreeAndSelectNext ::
  forall a b.
  (b -> a) -> ForestCursor a b -> Maybe (DeleteOrUpdate (ForestCursor a b))
forestCursorDeleteSubTreeAndSelectNext g fc =
  joinPossibleDeletes
    ( fc
        # focusPossibleDeleteOrUpdate forestCursorSelectedTreeL (treeCursorDeleteSubTreeAndSelectNext g)
    )
    ( fc
        # focusPossibleDeleteOrUpdate
            forestCursorListCursorL
            (nonEmptyCursorDeleteElemAndSelectNext (makeTreeCursor g))
    )

forestCursorRemoveSubTree :: forall a b. (b -> a) -> ForestCursor a b -> DeleteOrUpdate (ForestCursor a b)
forestCursorRemoveSubTree g fc =
  (fc # (traverseOf forestCursorSelectedTreeL) (treeCursorRemoveSubTree g))
    <|> (fc # (traverseOf forestCursorListCursorL) (nonEmptyCursorRemoveElem (makeTreeCursor g)))

forestCursorDeleteSubTree :: forall a b. (b -> a) -> ForestCursor a b -> DeleteOrUpdate (ForestCursor a b)
forestCursorDeleteSubTree g fc =
  (fc # (traverseOf forestCursorSelectedTreeL) (treeCursorDeleteSubTree g))
    <|> (fc # (traverseOf forestCursorListCursorL) (nonEmptyCursorDeleteElem (makeTreeCursor g)))

forestCursorAddRoot :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> a -> TreeCursor a b
forestCursorAddRoot f g fc v = makeTreeCursor g $ CTree { rootLabel: f v, subForest: OpenForest $ rebuildForestCursor f fc }

-- | Swaps the current node with the previous node on the same level
--
-- Example:
--
-- Before:
--
-- > - a
-- > - b <--
--
-- After:
--
-- > - b <--
-- > - a
forestCursorSwapPrev :: forall a b. ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSwapPrev fc@(ForestCursor ne) = case fc # (traverseOf forestCursorSelectedTreeL) treeCursorSwapPrev of
  Swapped fc' -> pure fc'
  NoSiblingsToSwapWith -> Nothing
  SwapperIsTopNode -> case ne.forestCursorListCursor.nonEmptyCursorPrev of
    Nil -> Nothing
    (t : ts) ->
      pure
        $ ForestCursor
            { forestCursorListCursor:
              ne.forestCursorListCursor
                { nonEmptyCursorPrev = ts, nonEmptyCursorNext = t : ne.forestCursorListCursor.nonEmptyCursorNext
                }
            }

-- | Swaps the current node with the next node on the same level
--
-- Example:
--
-- Before:
--
-- > - a <--
-- > - b
--
-- After:
--
-- > - b
-- > - a <--
forestCursorSwapNext :: forall a b. ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorSwapNext fc@(ForestCursor ne) = case fc # (traverseOf forestCursorSelectedTreeL) treeCursorSwapNext of
  Swapped fc' -> pure fc'
  NoSiblingsToSwapWith -> Nothing
  SwapperIsTopNode -> case ne.forestCursorListCursor.nonEmptyCursorNext of
    Nil -> Nothing
    (t : ts) ->
      pure
        $ ForestCursor
            { forestCursorListCursor: ne.forestCursorListCursor { nonEmptyCursorPrev = t : ne.forestCursorListCursor.nonEmptyCursorPrev, nonEmptyCursorNext = ts }
            }

-- | Promotes the current node to the level of its parent.
--
-- Example:
--
-- Before:
--
-- > - a
-- >   |- b
-- >   |  |- c
-- >   |- d <--
-- >   |  |- e
-- >   |- f
-- >      |- g
-- > - h
--
-- After:
--
-- > - a
-- >   |- b
-- >   |  |- c
-- >   |  |- e
-- >   |- f
-- >      |- g
-- > - d <--
-- > - h
forestCursorPromoteElem :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorPromoteElem f g fc@(ForestCursor ne) = case fc # (traverseOf forestCursorSelectedTreeL) (treeCursorPromoteElem f g) of
  PromotedElem fc' -> pure fc'
  CannotPromoteTopElem -> Nothing
  NoSiblingsToAdoptChildren -> Nothing
  NoGrandparentToPromoteElemUnder -> do
    let
      tc = fc ^. forestCursorSelectedTreeL
    TreeAbove ta <- tc.treeAbove
    lefts <- case tc.treeBelow of
      EmptyCForest -> pure $ ta.treeAboveLefts
      _ -> case ta.treeAboveLefts of
        Nil -> Nothing
        (CTree t : ts) ->
          pure
            $ CTree
                { rootLabel: t.rootLabel
                , subForest: (openForest $ unpackCForest t.subForest <> unpackCForest tc.treeBelow)
                }
            : ts
    let
      ta' = TreeAbove ta { treeAboveLefts = lefts }
    let
      tc' = tc { treeAbove = Just ta' }
    tc'' <- case treeCursorDeleteSubTree g tc' of
      Deleted -> Nothing -- Cannot happen, otherwise we would have gotten 'CannotPromoteTopTree'.
      Updated tc'' -> pure tc''
    pure
      $ ForestCursor
          { forestCursorListCursor:
            ne.forestCursorListCursor
              { nonEmptyCursorPrev = rebuildTreeCursor f tc'' : ne.forestCursorListCursor.nonEmptyCursorPrev
              , nonEmptyCursorCurrent =
                singletonTreeCursor $ (fc ^. forestCursorSelectedTreeL).treeCurrent
              }
          }

-- | Promotes the current node to the level of its parent.
--
-- Example:
--
-- Before:
--
-- >  - a
-- >    |- b
-- >    |  |- c
-- >    |- d <--
-- >    |  |- e
-- >    |- f
-- >       |- g
-- >  - h
--
-- After:
--
-- >
-- > - a
-- >   |- b
-- >   |  |- c
-- >   |- f
-- >      |- g
-- > - d <--
-- >   |- e
-- > - h
forestCursorPromoteSubTree :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorPromoteSubTree f g fc@(ForestCursor ne) = case fc # (traverseOf forestCursorSelectedTreeL) (treeCursorPromoteSubTree f g) of
  Promoted fc' -> pure fc'
  CannotPromoteTopNode -> Nothing
  NoGrandparentToPromoteUnder -> case treeCursorDeleteSubTree g $ fc ^. forestCursorSelectedTreeL of
    Deleted -> Nothing -- Cannot happen, otherwise we would have gotten 'CannotPromoteTopTree'.
    Updated tc' ->
      pure
        $ ForestCursor
            { forestCursorListCursor:
              ne.forestCursorListCursor
                { nonEmptyCursorPrev = rebuildTreeCursor f tc' : ne.forestCursorListCursor.nonEmptyCursorPrev
                , nonEmptyCursorCurrent = (fc ^. forestCursorSelectedTreeL) { treeAbove = Nothing }
                }
            }

-- | Demotes the current node to the level of its children.
--
-- Example:
--
-- Before:
--
-- > - a
-- >   |- b
-- > - c <--
-- >   |- d
-- > - e
--
-- After:
--
-- > - a
-- >   |- b
-- >   |- c <--
-- >   |- d
-- > - e
forestCursorDemoteElem :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorDemoteElem f g fc@(ForestCursor ne) = case fc # (traverseOf forestCursorSelectedTreeL) (treeCursorDemoteElem f g) of
  Demoted fc' -> pure fc'
  CannotDemoteTopNode -> case ne.forestCursorListCursor.nonEmptyCursorPrev of
    Nil -> Nothing
    (CTree t : ts) -> do
      let
        CTree t' = rebuildTreeCursor f (fc ^. forestCursorSelectedTreeL)
      let
        n' =
          CTree
            { rootLabel: t.rootLabel
            , subForest:
              openForest
                $ unpackCForest t.subForest
                <> CTree { rootLabel: t'.rootLabel, subForest: emptyCForest }
                : unpackCForest t'.subForest
            }
      tc <- makeTreeCursorWithSelection f g (SelectChild (lengthCForest t.subForest) SelectNode) n'
      pure
        $ ForestCursor
            { forestCursorListCursor:
              ne.forestCursorListCursor
                { nonEmptyCursorPrev = ts, nonEmptyCursorCurrent = tc
                }
            }
  NoSiblingsToDemoteUnder -> Nothing

-- | Demotes the current subtree to the level of its children.
--
-- Example:
--
-- Before:
--
-- >  - a
-- >    |- b
-- >  - c <--
-- >    |- d
--
-- After:
--
-- >  - a
-- >    |- b
-- >    |- c <--
-- >       |- d
forestCursorDemoteSubTree :: forall a b. (a -> b) -> (b -> a) -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorDemoteSubTree f g fc@(ForestCursor ne) = case fc # (traverseOf forestCursorSelectedTreeL) (treeCursorDemoteSubTree f g) of
  Demoted fc' -> pure fc'
  CannotDemoteTopNode -> case ne.forestCursorListCursor.nonEmptyCursorPrev of
    Nil -> Nothing
    (CTree t : ts) -> do
      let
        n' =
          CTree
            { rootLabel: t.rootLabel
            , subForest:
              openForest
                $ unpackCForest t.subForest
                <> (rebuildTreeCursor f (fc ^. forestCursorSelectedTreeL) : Nil)
            }
      tc <- makeTreeCursorWithSelection f g (SelectChild (lengthCForest t.subForest) SelectNode) n'
      pure
        $ ForestCursor
            { forestCursorListCursor:
              ne.forestCursorListCursor
                { nonEmptyCursorPrev = ts, nonEmptyCursorCurrent = tc
                }
            }
  NoSiblingsToDemoteUnder -> Nothing

-- | Demotes the current node to the level of its children, by adding two roots.
-- One for the current node and one for its children that are left behind.
--
-- Example:
--
-- Before:
--
-- >  - a <--
-- >    |- b
--
-- After:
--
-- >  - <given element 1>
-- >    |- a <--
-- >  - <given element 2>
-- >    |- b
forestCursorDemoteElemUnder :: forall a b. b -> b -> ForestCursor a b -> ForestCursor a b
forestCursorDemoteElemUnder b1 b2 fc@(ForestCursor ne) = case fc # (traverseOf forestCursorSelectedTreeL) (treeCursorDemoteElemUnder b1 b2) of
  Just fc' -> fc'
  Nothing ->
    let
      t = fc ^. forestCursorSelectedTreeL
    in
      ForestCursor
        { forestCursorListCursor:
          ne.forestCursorListCursor
            { nonEmptyCursorCurrent =
              { treeAbove:
                Just
                  ( TreeAbove
                      { treeAboveLefts: Nil
                      , treeAboveAbove: Nothing
                      , treeAboveNode: b1
                      , treeAboveRights: Nil
                      }
                  )
              , treeCurrent: t.treeCurrent
              , treeBelow: emptyCForest
              }
            , nonEmptyCursorNext = CTree { rootLabel: b2, subForest: t.treeBelow } : ne.forestCursorListCursor.nonEmptyCursorNext
            }
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
forestCursorDemoteSubTreeUnder :: forall a b. b -> ForestCursor a b -> ForestCursor a b
forestCursorDemoteSubTreeUnder b = forestCursorSelectedTreeL %~ treeCursorDemoteSubTreeUnder b

traverseForestCursor :: forall a b c f. (List (CTree b) -> TreeCursor a b -> List (CTree b) -> f c) -> ForestCursor a b -> f c
traverseForestCursor = foldForestCursor

foldForestCursor :: forall a b c. (List (CTree b) -> TreeCursor a b -> List (CTree b) -> c) -> ForestCursor a b -> c
foldForestCursor func (ForestCursor ne) = foldNonEmptyCursor func ne.forestCursorListCursor

forestCursorMoveUsingPath :: forall a b. (a -> b) -> (b -> a) -> PathToClickedEntry -> ForestCursor a b -> Maybe (ForestCursor a b)
forestCursorMoveUsingPath f g p fc = case p of
  ClickedEqualsSelected -> Just fc
  GoToSibling index p' ->
    forestCursorMoveUsingPath f g p' fc
      >>= ( \fc' -> case forestCursorSelectAbove f g fc' of
            Nothing -> forestCursorSelectIndex f g index fc -- Already on the top level
            Just fc'' -> forestCursorSelectBelowAtPos f g index fc''
        )
  GoToParent p' -> forestCursorSelectAbove f g =<< forestCursorMoveUsingPath f g p' fc
  GoToChild index p' -> forestCursorSelectBelowAtPos f g index =<< forestCursorMoveUsingPath f g p' fc
