{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Smos.Cursor.Collapse
    ( Collapse
    , makeCollapse
    , rebuildCollapse
    , collapseValueL
    , collapseCollapseTreeL
    , collapseCollapseEntryL
    , collapseShowSubForestL
    , collapseShowContentsL
    , CollapseTree(..)
    , makeCollapseTree
    , rebuildCollapseTree
    , collapseTreeValueL
    , collapseTreeShowSubForestL
    , CollapseEntry(..)
    , makeCollapseEntry
    , rebuildCollapseEntry
    , collapseEntryValueL
    , collapseEntryShowContentsL
    ) where

import GHC.Generics (Generic)

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Validity

import Lens.Micro

import Cursor.Forest
import Cursor.Tree
import Cursor.Types

import Smos.Data.Types

import Smos.Cursor.Entry

type Collapse a = CollapseTree (CollapseEntry a)

makeCollapse :: a -> Collapse a
makeCollapse = makeCollapseTree . makeCollapseEntry

rebuildCollapse :: Collapse a -> a
rebuildCollapse = rebuildCollapseEntry . rebuildCollapseTree

collapseValueL :: Lens (Collapse a) (Collapse b) a b
collapseValueL = collapseTreeValueL . collapseEntryValueL

collapseCollapseTreeL ::
       Lens (Collapse a) (Collapse b) (CollapseTree (CollapseEntry a)) (CollapseTree (CollapseEntry b))
collapseCollapseTreeL = id

collapseCollapseEntryL ::
       Lens (Collapse a) (Collapse b) (CollapseEntry a) (CollapseEntry b)
collapseCollapseEntryL = collapseTreeValueL

collapseShowSubForestL :: Lens' (Collapse a) Bool
collapseShowSubForestL = collapseTreeShowSubForestL

collapseShowContentsL :: Lens' (Collapse a) Bool
collapseShowContentsL = collapseCollapseEntryL . collapseEntryShowContentsL

data CollapseTree a = CollapseTree
    { collapseTreeValue :: a
    , collapseTreeShowSubForest :: Bool
    } deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (CollapseTree a)

makeCollapseTree :: a -> CollapseTree a
makeCollapseTree a =
    CollapseTree {collapseTreeValue = a, collapseTreeShowSubForest = True}

rebuildCollapseTree :: CollapseTree a -> a
rebuildCollapseTree = collapseTreeValue

collapseTreeValueL :: Lens (CollapseTree a) (CollapseTree b) a b
collapseTreeValueL =
    lens collapseTreeValue $ \ct v -> ct {collapseTreeValue = v}

collapseTreeShowSubForestL :: Lens' (CollapseTree a) Bool
collapseTreeShowSubForestL =
    lens collapseTreeShowSubForest $ \ct b -> ct {collapseTreeShowSubForest = b}

data CollapseEntry a = CollapseEntry
    { collapseEntryValue :: a
    , collapseEntryShowContents :: Bool
    } deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (CollapseEntry a)

makeCollapseEntry :: a -> CollapseEntry a
makeCollapseEntry a =
    CollapseEntry {collapseEntryValue = a, collapseEntryShowContents = True}

rebuildCollapseEntry :: CollapseEntry a -> a
rebuildCollapseEntry = collapseEntryValue

collapseEntryValueL :: Lens (CollapseEntry a) (CollapseEntry b) a b
collapseEntryValueL =
    lens collapseEntryValue $ \ct v -> ct {collapseEntryValue = v}

collapseEntryShowContentsL :: Lens' (CollapseEntry a) Bool
collapseEntryShowContentsL =
    lens collapseEntryShowContents $ \ct b -> ct {collapseEntryShowContents = b}
