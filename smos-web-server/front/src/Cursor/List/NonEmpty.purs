module Cursor.List.NonEmpty where

import Prelude
import Data.List (List(..), foldr, length, reverse, snoc, (:))
import Control.Alt ((<|>))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.NonEmpty ((:|))
import Data.List.NonEmpty as NE
import Cursor.Types (DeleteOrUpdate(..), joinDeletes)
import Data.Maybe (Maybe(..))
import Data.Lens (Lens, lens)

-- | A 'nonempty list' cursor
type NonEmptyCursor a b
  = { nonEmptyCursorPrev :: List b
    , nonEmptyCursorCurrent :: a
    , nonEmptyCursorNext :: List b
    }

makeNonEmptyCursor :: forall a b. (b -> a) -> NonEmptyList b -> NonEmptyCursor a b
makeNonEmptyCursor g (NonEmptyList (b :| bs)) =
  { nonEmptyCursorPrev: Nil
  , nonEmptyCursorCurrent: g b
  , nonEmptyCursorNext: bs
  }

makeNonEmptyCursorWithSelection :: forall a b. (b -> a) -> Int -> NonEmptyList b -> Maybe (NonEmptyCursor a b)
makeNonEmptyCursorWithSelection g i ne = do
  triple <- applyNonEmptyListSelection ne i
  pure
    { nonEmptyCursorPrev: reverse triple.lefts
    , nonEmptyCursorCurrent: g triple.middle
    , nonEmptyCursorNext: triple.rights
    }
  where
  applyNonEmptyListSelection :: forall c. NonEmptyList c -> Int -> Maybe { lefts :: List c, middle :: c, rights :: List c }
  applyNonEmptyListSelection (NonEmptyList (c :| rest)) i_
    | i_ < 0 = Nothing
    | i_ == 0 = Just { lefts: Nil, middle: c, rights: rest }
    | otherwise = do
      ne_ <- NE.fromList rest
      triple <- applyNonEmptyListSelection ne_ (i_ - 1)
      pure triple { lefts = c : triple.lefts }

singletonNonEmptyCursor :: forall a b. a -> NonEmptyCursor a b
singletonNonEmptyCursor a =
  { nonEmptyCursorPrev: Nil
  , nonEmptyCursorCurrent: a
  , nonEmptyCursorNext: Nil
  }

rebuildNonEmptyCursor :: forall a b. (a -> b) -> NonEmptyCursor a b -> NonEmptyList b
rebuildNonEmptyCursor f nec =
  nonemptyPrepend (reverse nec.nonEmptyCursorPrev)
    $ NonEmptyList
    $ f nec.nonEmptyCursorCurrent
    :| nec.nonEmptyCursorNext

mapNonEmptyCursor :: forall a b c d. (a -> c) -> (b -> d) -> NonEmptyCursor a b -> NonEmptyCursor c d
mapNonEmptyCursor f g nec =
  { nonEmptyCursorPrev: map g nec.nonEmptyCursorPrev
  , nonEmptyCursorCurrent: f nec.nonEmptyCursorCurrent
  , nonEmptyCursorNext: map g nec.nonEmptyCursorNext
  }

nonEmptyCursorElemL :: forall a b c. Lens (NonEmptyCursor a c) (NonEmptyCursor b c) a b
nonEmptyCursorElemL = lens _.nonEmptyCursorCurrent $ \nec le -> nec { nonEmptyCursorCurrent = le }

nonEmptyCursorSelectPrev :: forall a b. (a -> b) -> (b -> a) -> NonEmptyCursor a b -> Maybe (NonEmptyCursor a b)
nonEmptyCursorSelectPrev f g nec = case nec.nonEmptyCursorPrev of
  Nil -> Nothing
  (e : rest) ->
    Just
      $ nec
          { nonEmptyCursorPrev = rest
          , nonEmptyCursorCurrent = g e
          , nonEmptyCursorNext = f nec.nonEmptyCursorCurrent : nec.nonEmptyCursorNext
          }

nonEmptyCursorSelectNext :: forall a b. (a -> b) -> (b -> a) -> NonEmptyCursor a b -> Maybe (NonEmptyCursor a b)
nonEmptyCursorSelectNext f g nec = case nec.nonEmptyCursorNext of
  Nil -> Nothing
  (e : rest) ->
    Just
      $ nec
          { nonEmptyCursorPrev = f nec.nonEmptyCursorCurrent : nec.nonEmptyCursorPrev
          , nonEmptyCursorCurrent = g e
          , nonEmptyCursorNext = rest
          }

nonEmptyCursorSelectFirst :: forall a b. (a -> b) -> (b -> a) -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorSelectFirst f g nec = case nonEmptyCursorSelectPrev f g nec of
  Nothing -> nec
  Just nec' -> nonEmptyCursorSelectFirst f g nec'

nonEmptyCursorSelectLast :: forall a b. (a -> b) -> (b -> a) -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorSelectLast f g nec = case nonEmptyCursorSelectNext f g nec of
  Nothing -> nec
  Just nec' -> nonEmptyCursorSelectLast f g nec'

nonEmptyCursorSelection :: forall a b. NonEmptyCursor a b -> Int
nonEmptyCursorSelection = length <<< _.nonEmptyCursorPrev

nonEmptyCursorSelectIndex ::
  forall a b.
  (a -> b) -> (b -> a) -> Int -> NonEmptyCursor a b -> Maybe (NonEmptyCursor a b)
nonEmptyCursorSelectIndex f g i nec
  | i < nonEmptyCursorSelection nec = nonEmptyCursorSelectPrev f g nec >>= nonEmptyCursorSelectIndex f g i
  | i > nonEmptyCursorSelection nec = nonEmptyCursorSelectNext f g nec >>= nonEmptyCursorSelectIndex f g i
  | otherwise = Just nec

nonEmptyCursorInsert :: forall a b. b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorInsert c nec = nec { nonEmptyCursorPrev = c : nec.nonEmptyCursorPrev }

nonEmptyCursorAppend :: forall a b. b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorAppend c nec = nec { nonEmptyCursorNext = c : nec.nonEmptyCursorNext }

nonEmptyCursorInsertAndSelect :: forall a b. (a -> b) -> a -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorInsertAndSelect f c nec =
  nec
    { nonEmptyCursorCurrent = c
    , nonEmptyCursorNext = f nec.nonEmptyCursorCurrent : nec.nonEmptyCursorNext
    }

nonEmptyCursorAppendAndSelect :: forall a b. (a -> b) -> a -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorAppendAndSelect f c nec =
  nec
    { nonEmptyCursorCurrent = c
    , nonEmptyCursorPrev = f nec.nonEmptyCursorCurrent : nec.nonEmptyCursorPrev
    }

nonEmptyCursorInsertAtStart :: forall a b. b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorInsertAtStart c nec = nec { nonEmptyCursorPrev = snoc (nec.nonEmptyCursorPrev) c }

nonEmptyCursorAppendAtEnd :: forall a b. b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorAppendAtEnd c nec = nec { nonEmptyCursorNext = snoc (nec.nonEmptyCursorNext) c }

nonEmptyCursorInsertAtStartAndSelect ::
  forall a b.
  (a -> b) -> (b -> a) -> b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorInsertAtStartAndSelect f g c = nonEmptyCursorSelectFirst f g <<< nonEmptyCursorInsertAtStart c

nonEmptyCursorAppendAtEndAndSelect ::
  forall a b.
  (a -> b) -> (b -> a) -> b -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorAppendAtEndAndSelect f g c = nonEmptyCursorSelectLast f g <<< nonEmptyCursorAppendAtEnd c

nonEmptyCursorRemoveElemAndSelectPrev ::
  forall a b.
  (b -> a) -> NonEmptyCursor a b -> Maybe (DeleteOrUpdate (NonEmptyCursor a b))
nonEmptyCursorRemoveElemAndSelectPrev g nec = case nec.nonEmptyCursorPrev of
  Nil -> case nec.nonEmptyCursorNext of
    Nil -> Just Deleted
    _ -> Nothing
  (e : rest) -> Just $ Updated $ nec { nonEmptyCursorPrev = rest, nonEmptyCursorCurrent = g e }

nonEmptyCursorDeleteElemAndSelectNext ::
  forall a b.
  (b -> a) -> NonEmptyCursor a b -> Maybe (DeleteOrUpdate (NonEmptyCursor a b))
nonEmptyCursorDeleteElemAndSelectNext g nec = case nec.nonEmptyCursorNext of
  Nil -> case nec.nonEmptyCursorPrev of
    Nil -> Just Deleted
    _ -> Nothing
  (e : rest) -> Just $ Updated $ nec { nonEmptyCursorCurrent = g e, nonEmptyCursorNext = rest }

nonEmptyCursorRemoveElem :: forall a b. (b -> a) -> NonEmptyCursor a b -> DeleteOrUpdate (NonEmptyCursor a b)
nonEmptyCursorRemoveElem g nec =
  joinDeletes
    (nonEmptyCursorRemoveElemAndSelectPrev g nec)
    (nonEmptyCursorDeleteElemAndSelectNext g nec)

nonEmptyCursorDeleteElem :: forall a b. (b -> a) -> NonEmptyCursor a b -> DeleteOrUpdate (NonEmptyCursor a b)
nonEmptyCursorDeleteElem g nec =
  joinDeletes
    (nonEmptyCursorDeleteElemAndSelectNext g nec)
    (nonEmptyCursorRemoveElemAndSelectPrev g nec)

nonEmptyCursorSearch ::
  forall a b.
  (a -> b) -> (b -> a) -> (a -> Boolean) -> NonEmptyCursor a b -> Maybe (NonEmptyCursor a b)
nonEmptyCursorSearch f g p nec =
  if p nec.nonEmptyCursorCurrent then
    Just nec
  else
    lookPrev nec <|> lookNext nec
  where
  lookPrev = look nonEmptyCursorSelectPrev

  lookNext = look nonEmptyCursorSelectNext

  look func nec_ = do
    nec' <- func f g nec_
    if p $ nec'.nonEmptyCursorCurrent then
      Just nec'
    else
      look func nec'

nonEmptyCursorSelectOrAdd ::
  forall a b.
  (a -> b) -> (b -> a) -> (a -> Boolean) -> a -> NonEmptyCursor a b -> NonEmptyCursor a b
nonEmptyCursorSelectOrAdd f g p a nec = case nonEmptyCursorSearch f g p nec of
  Nothing -> nonEmptyCursorAppendAndSelect f a nec
  Just nec' -> nec'

nonemptyPrepend :: forall a. List a -> NonEmptyList a -> NonEmptyList a
nonemptyPrepend ls ne = foldr NE.cons ne ls

nonemptyAppend :: forall a. NonEmptyList a -> List a -> NonEmptyList a
nonemptyAppend (NonEmptyList (x :| xs)) ls = NonEmptyList $ x :| (xs <> ls)

traverseNonEmptyCursor :: forall a b c f. (List b -> a -> List b -> f c) -> NonEmptyCursor a b -> f c
traverseNonEmptyCursor = foldNonEmptyCursor

foldNonEmptyCursor :: forall a b c. (List b -> a -> List b -> c) -> NonEmptyCursor a b -> c
foldNonEmptyCursor func nec = func (reverse nec.nonEmptyCursorPrev) nec.nonEmptyCursorCurrent nec.nonEmptyCursorNext
