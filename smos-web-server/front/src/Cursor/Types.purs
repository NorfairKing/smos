module Cursor.Types where

import Prelude
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Control.Plus (class Alt, (<|>))
import Data.Lens (Lens', traverseOf)

data DeleteOrUpdate a
  = Deleted
  | Updated a

instance functorDeleteOrUpdate :: Functor DeleteOrUpdate where
  map _ Deleted = Deleted
  map f (Updated a) = Updated (f a)

instance applyDeleteOrUpdate :: Apply DeleteOrUpdate where
  apply Deleted _ = Deleted
  apply _ Deleted = Deleted
  apply (Updated f) (Updated a) = Updated (f a)

instance applicativeDeleteOrUpdate :: Applicative DeleteOrUpdate where
  pure = Updated

instance altDeleteOrUpdate :: Alt DeleteOrUpdate where
  alt (Updated a) _ = Updated a
  alt Deleted doua = doua

joinDeletes :: forall a. Maybe (DeleteOrUpdate a) -> Maybe (DeleteOrUpdate a) -> DeleteOrUpdate a
joinDeletes m1 m2 = case m1 of
  Nothing -> case m2 of
    Nothing -> Deleted
    Just a -> a
  Just a -> a

joinDeletes3 ::
  forall a.
  Maybe (DeleteOrUpdate a) ->
  Maybe (DeleteOrUpdate a) ->
  Maybe (DeleteOrUpdate a) ->
  DeleteOrUpdate a
joinDeletes3 m1 m2 m3 = case m1 of
  Nothing -> case m2 of
    Nothing -> case m3 of
      Nothing -> Deleted
      Just a -> a
    Just a -> a
  Just a -> a

joinPossibleDeletes ::
  forall a.
  Maybe (DeleteOrUpdate a) -> Maybe (DeleteOrUpdate a) -> Maybe (DeleteOrUpdate a)
joinPossibleDeletes d1 d2 = unwrap $ Compose d1 <|> Compose d2

focusPossibleDeleteOrUpdate ::
  forall a b.
  Lens' a b -> (b -> Maybe (DeleteOrUpdate b)) -> a -> Maybe (DeleteOrUpdate a)
focusPossibleDeleteOrUpdate l func = unwrap <<< (traverseOf l) (Compose <<< func)

dullMDelete :: forall a. Maybe (DeleteOrUpdate a) -> Maybe a
dullMDelete = case _ of
  Nothing -> Nothing
  Just dou -> dullDelete dou

dullDelete :: forall a. DeleteOrUpdate a -> Maybe a
dullDelete = case _ of
  Deleted -> Nothing
  Updated a -> Just a
