{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Smos.Cursor.Collapse
    ( Collapse
    , makeCollapse
    , rebuildCollapse
    , collapseValueL
    , CollapseTree(..)
    , makeCollapseTree
    , rebuildCollapseTree
    , collapseTreeValueL
    , CollapseEntry(..)
    , makeCollapseEntry
    , rebuildCollapseEntry
    , collapseEntryValueL
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
collapseValueL :: Lens' (Collapse a) a
collapseValueL = collapseTreeValueL.collapseEntryValueL

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

collapseTreeValueL :: Lens' (CollapseTree a) a
collapseTreeValueL =
    lens collapseTreeValue $ \ct v -> ct {collapseTreeValue = v}

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

collapseEntryValueL :: Lens' (CollapseEntry a) a
collapseEntryValueL =
    lens collapseEntryValue $ \ct v -> ct {collapseEntryValue = v}
