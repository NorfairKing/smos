{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Smos.Cursor.Collapse
    ( CollapseEntry(..)
    , makeCollapseEntry
    , rebuildCollapseEntry
    , collapseEntryValueL
    , collapseEntryShowContentsL
    , collapseEntryShowHistoryL
    , collapseEntrySetShowAll
    ) where

import GHC.Generics (Generic)

import Data.Validity

import Lens.Micro

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
collapseEntrySetShowAll b e =
    e & collapseEntryShowContentsL .~ b & collapseEntryShowHistoryL .~ b
