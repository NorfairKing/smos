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
    , collapseShowHistoryL
    , CollapseCycle(..)
    , collapseCycle
    , setCollapseCycle
    , runCollapseCycle
    , collapseSetShowEntireEntry
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
    , collapseEntryShowHistoryL
    , collapseEntrySetShowAll
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

collapseShowHistoryL :: Lens' (Collapse a) Bool
collapseShowHistoryL = collapseCollapseEntryL . collapseEntryShowHistoryL

data CollapseCycle
    = ShowOnlyHeader
    | ShowHeaderAndChildren
    | ShowAll
    deriving (Show, Eq, Generic)

instance Validity CollapseCycle

-- | Circular, because it's a cycle
instance Enum CollapseCycle where
    toEnum n =
        case n `rem` 3 of
            0 -> ShowOnlyHeader
            1 -> ShowHeaderAndChildren
            2 -> ShowAll
    fromEnum ShowOnlyHeader = 0
    fromEnum ShowHeaderAndChildren = 1
    fromEnum ShowAll = 2

collapseCycle :: Collapse a -> CollapseCycle
collapseCycle c
    | c ^. collapseShowSubForestL &&
          c ^. collapseShowContentsL && c ^. collapseShowHistoryL = ShowAll
    | c ^. collapseShowSubForestL = ShowHeaderAndChildren
    | otherwise = ShowOnlyHeader

setCollapseCycle :: Collapse a -> CollapseCycle -> Collapse a
setCollapseCycle c ShowAll =
    c & collapseShowSubForestL .~ True & collapseShowContentsL .~ True &
    collapseShowHistoryL .~ True
setCollapseCycle c ShowHeaderAndChildren =
    c & collapseShowSubForestL .~ True & collapseShowContentsL .~ False &
    collapseShowHistoryL .~ False
setCollapseCycle c ShowOnlyHeader =
    c & collapseShowSubForestL .~ False & collapseShowContentsL .~ False &
    collapseShowHistoryL .~ False

runCollapseCycle :: Collapse a -> Collapse a
runCollapseCycle c =
    let cc = collapseCycle c
        cc' = succ cc
     in setCollapseCycle c cc'

collapseSetShowEntireEntry :: Bool -> Collapse a -> Collapse a
collapseSetShowEntireEntry b =
    collapseCollapseEntryL %~ collapseEntrySetShowAll b

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
    , collapseEntryShowHistory :: Bool
    } deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (CollapseEntry a)

makeCollapseEntry :: a -> CollapseEntry a
makeCollapseEntry a =
    CollapseEntry
        { collapseEntryValue = a
        , collapseEntryShowContents = True
        , collapseEntryShowHistory = False
        }

rebuildCollapseEntry :: CollapseEntry a -> a
rebuildCollapseEntry = collapseEntryValue

collapseEntryValueL :: Lens (CollapseEntry a) (CollapseEntry b) a b
collapseEntryValueL =
    lens collapseEntryValue $ \ct v -> ct {collapseEntryValue = v}

collapseEntryShowContentsL :: Lens' (CollapseEntry a) Bool
collapseEntryShowContentsL =
    lens collapseEntryShowContents $ \ct b -> ct {collapseEntryShowContents = b}

collapseEntryShowHistoryL :: Lens' (CollapseEntry a) Bool
collapseEntryShowHistoryL =
    lens collapseEntryShowHistory $ \ct b -> ct {collapseEntryShowHistory = b}

collapseEntrySetShowAll :: Bool -> CollapseEntry a -> CollapseEntry a
collapseEntrySetShowAll b e  =
    e & collapseEntryShowContentsL .~ b & collapseEntryShowHistoryL .~ b
