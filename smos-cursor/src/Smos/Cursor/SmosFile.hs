module Smos.Cursor.SmosFile
    ( SmosFileCursor
    , makeSmosFileCursor
    , rebuildSmosFileCursor
    , startSmosFile
    , smosFileCursorSelectedEntryL
    , smosFileCursorEntrySelectionL
    , smosFileCursorToggleHideSubForest
    , smosFileCursorToggleHideEntireEntry
    , smosFileCursorRunCollapseCycle
    , smosFileCursorSelectPrev
    , smosFileCursorSelectNext
    , smosFileCursorSelectPrevOnSameLevel
    , smosFileCursorSelectNextOnSameLevel
    , smosFileCursorInsertEntryBefore
    , smosFileCursorInsertEntryBeforeAndSelectHeader
    , smosFileCursorInsertEntryBelow
    , smosFileCursorInsertEntryBelowAndSelectHeader
    , smosFileCursorInsertEntryAfter
    , smosFileCursorInsertEntryAfterAndSelectHeader
    , smosFileCursorDeleteElem
    , smosFileCursorDeleteSubTree
    , smosFileCursorSwapPrev
    , smosFileCursorSwapNext
    , smosFileCursorPromoteEntry
    , smosFileCursorPromoteSubTree
    , smosFileCursorDemoteEntry
    , smosFileCursorDemoteSubTree
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe

import Lens.Micro

import Cursor.Forest
import Cursor.Tree
import Cursor.Types

import Smos.Data.Types

import Smos.Cursor.Collapse
import Smos.Cursor.Entry

type SmosFileCursor = ForestCursor (Collapse EntryCursor) (Collapse Entry)

makeSmosFileCursor :: NonEmpty (Tree Entry) -> SmosFileCursor
makeSmosFileCursor =
    makeForestCursor (collapseValueL %~ makeEntryCursor) .
    NE.map (fmap makeCollapse)

rebuildSmosFileCursor :: SmosFileCursor -> NonEmpty (Tree Entry)
rebuildSmosFileCursor =
    NE.map (fmap rebuildCollapse) .
    rebuildForestCursor (collapseValueL %~ rebuildEntryCursor)

startSmosFile :: SmosFileCursor
startSmosFile =
    (smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtStart) $
    makeSmosFileCursor $ Node emptyEntry [] :| []

smosFileCursorSelectedEntireL :: Lens' SmosFileCursor (Collapse EntryCursor)
smosFileCursorSelectedEntireL = forestCursorSelectedTreeL . treeCursorCurrentL

smosFileCursorSelectedEntryL :: Lens' SmosFileCursor EntryCursor
smosFileCursorSelectedEntryL = smosFileCursorSelectedEntireL . collapseValueL

smosFileCursorEntrySelectionL :: Lens' SmosFileCursor EntryCursorSelection
smosFileCursorEntrySelectionL =
    smosFileCursorSelectedEntryL . entryCursorSelectionL

smosFileCursorToggleHideSubForest :: SmosFileCursor -> SmosFileCursor
smosFileCursorToggleHideSubForest =
    (smosFileCursorSelectedEntireL . collapseShowSubForestL) %~ not

smosFileCursorToggleHideEntireEntry :: SmosFileCursor -> SmosFileCursor
smosFileCursorToggleHideEntireEntry =
    smosFileCursorSelectedEntireL %~
    (\c ->
         collapseSetShowEntireEntry
             (not $ c ^. collapseShowContentsL && c ^. collapseShowHistoryL)
             c)

smosFileCursorRunCollapseCycle :: SmosFileCursor -> SmosFileCursor
smosFileCursorRunCollapseCycle =
    smosFileCursorSelectedEntireL %~ runCollapseCycle

smosFileCursorSelectPrev :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectPrev = forestCursorSelectPrev rebuild make

smosFileCursorSelectNext :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectNext = forestCursorSelectNext rebuild make

smosFileCursorSelectPrevOnSameLevel :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectPrevOnSameLevel =
    forestCursorSelectPrevOnSameLevel rebuild make

smosFileCursorSelectNextOnSameLevel :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectNextOnSameLevel =
    forestCursorSelectNextOnSameLevel rebuild make

smosFileCursorInsertEntryBefore :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBefore = forestCursorInsert (makeCollapse emptyEntry)

smosFileCursorInsertEntryBeforeAndSelectHeader ::
       SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBeforeAndSelectHeader =
    (smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtStart) .
    fromJust .
    smosFileCursorSelectPrevOnSameLevel . smosFileCursorInsertEntryBefore

smosFileCursorInsertEntryBelow :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelow =
    forestCursorAddChildToNodeAtStart (makeCollapse emptyEntry)

smosFileCursorInsertEntryBelowAndSelectHeader ::
       SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelowAndSelectHeader =
    (smosFileCursorEntrySelectionL .~ HeaderSelected) .
    fromJust .
    forestCursorSelectBelowAtStart rebuild make . smosFileCursorInsertEntryBelow

smosFileCursorInsertEntryAfter :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryAfter = forestCursorAppend (makeCollapse emptyEntry)

smosFileCursorInsertEntryAfterAndSelectHeader ::
       SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryAfterAndSelectHeader =
    (smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtStart) .
    fromJust .
    smosFileCursorSelectNextOnSameLevel . smosFileCursorInsertEntryAfter

smosFileCursorDeleteSubTree :: SmosFileCursor -> DeleteOrUpdate SmosFileCursor
smosFileCursorDeleteSubTree = forestCursorDeleteSubTree make

smosFileCursorDeleteElem :: SmosFileCursor -> DeleteOrUpdate SmosFileCursor
smosFileCursorDeleteElem = forestCursorDeleteElem make

smosFileCursorSwapPrev :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSwapPrev = forestCursorSwapPrev

smosFileCursorSwapNext :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSwapNext = forestCursorSwapNext

smosFileCursorPromoteEntry :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorPromoteEntry = forestCursorPromoteElem rebuild make

smosFileCursorPromoteSubTree :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorPromoteSubTree = forestCursorPromoteSubTree rebuild make

smosFileCursorDemoteEntry :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorDemoteEntry = forestCursorDemoteElem rebuild make

smosFileCursorDemoteSubTree :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorDemoteSubTree = forestCursorDemoteSubTree rebuild make

rebuild :: Collapse EntryCursor -> Collapse Entry
rebuild = collapseValueL %~ rebuildEntryCursor

make :: Collapse Entry -> Collapse EntryCursor
make = collapseValueL %~ makeEntryCursor
