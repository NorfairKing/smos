{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.Cursor.Collapse
  ( CollapseEntry (..),
    makeCollapseEntry,
    rebuildCollapseEntry,
    collapseEntryValueL,
    collapseEntryShowContentsL,
    collapseEntryShowHistoryL,
    collapseEntryShowLogbookL,
    collapseEntryShowTimestampsL,
    collapseEntryShowPropertiesL,
    collapseEntrySetShowAll,
  )
where

import Control.DeepSeq
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro

data CollapseEntry a = CollapseEntry
  { collapseEntryValue :: a,
    collapseEntryShowContents :: Bool,
    collapseEntryShowHistory :: Bool,
    collapseEntryShowLogbook :: Bool,
    collapseEntryShowTimestamps :: Bool,
    collapseEntryShowProperties :: Bool
  }
  deriving (Show, Eq, Generic, Functor)

instance (Validity a) => Validity (CollapseEntry a)

instance (NFData a) => NFData (CollapseEntry a)

makeCollapseEntry :: a -> CollapseEntry a
makeCollapseEntry !a =
  CollapseEntry
    { collapseEntryValue = a,
      collapseEntryShowContents = True,
      collapseEntryShowHistory = False,
      collapseEntryShowLogbook = False,
      collapseEntryShowTimestamps = True,
      collapseEntryShowProperties = True
    }

rebuildCollapseEntry :: CollapseEntry a -> a
rebuildCollapseEntry = collapseEntryValue

collapseEntryValueL :: Lens (CollapseEntry a) (CollapseEntry b) a b
collapseEntryValueL = lens collapseEntryValue $ \ct v -> ct {collapseEntryValue = v}

collapseEntryShowContentsL :: Lens' (CollapseEntry a) Bool
collapseEntryShowContentsL =
  lens collapseEntryShowContents $ \ct b -> ct {collapseEntryShowContents = b}

collapseEntryShowHistoryL :: Lens' (CollapseEntry a) Bool
collapseEntryShowHistoryL =
  lens collapseEntryShowHistory $ \ct b -> ct {collapseEntryShowHistory = b}

collapseEntryShowLogbookL :: Lens' (CollapseEntry a) Bool
collapseEntryShowLogbookL =
  lens collapseEntryShowLogbook $ \ct b -> ct {collapseEntryShowLogbook = b}

collapseEntryShowTimestampsL :: Lens' (CollapseEntry a) Bool
collapseEntryShowTimestampsL =
  lens collapseEntryShowTimestamps $ \ct b -> ct {collapseEntryShowTimestamps = b}

collapseEntryShowPropertiesL :: Lens' (CollapseEntry a) Bool
collapseEntryShowPropertiesL =
  lens collapseEntryShowProperties $ \ct b -> ct {collapseEntryShowProperties = b}

collapseEntrySetShowAll :: Bool -> CollapseEntry a -> CollapseEntry a
collapseEntrySetShowAll b e =
  let CollapseEntry _ _ _ _ _ _ = undefined
   in e
        { collapseEntryShowContents = b,
          collapseEntryShowHistory = b,
          collapseEntryShowLogbook = b,
          collapseEntryShowProperties = b,
          collapseEntryShowTimestamps = b
        }
