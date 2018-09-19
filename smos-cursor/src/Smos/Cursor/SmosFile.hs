module Smos.Cursor.SmosFile
    ( SmosFileCursor
    , makeSmosFileCursor
    , rebuildSmosFileCursor
    , startSmosFile
    , smosFileCursorSelectedEntryL
    , smosFileCursorEntrySelectionL
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
import Data.Maybe

import Lens.Micro

import Cursor.Forest
import Cursor.Tree
import Cursor.Types

import Smos.Data.Types

import Smos.Cursor.Collapse
import Smos.Cursor.Entry

type SmosFileCursor = ForestCursor EntryCursor Entry

makeSmosFileCursor :: NonEmpty (Tree Entry) -> SmosFileCursor
makeSmosFileCursor = makeForestCursor makeEntryCursor

rebuildSmosFileCursor :: SmosFileCursor -> NonEmpty (Tree Entry)
rebuildSmosFileCursor = rebuildForestCursor rebuildEntryCursor

startSmosFile :: SmosFileCursor
startSmosFile =
    (smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtStart) $
    makeSmosFileCursor $ Node emptyEntry [] :| []

smosFileCursorSelectedEntryL :: Lens' SmosFileCursor EntryCursor
smosFileCursorSelectedEntryL = forestCursorSelectedTreeL . treeCursorCurrentL

smosFileCursorEntrySelectionL :: Lens' SmosFileCursor EntryCursorSelection
smosFileCursorEntrySelectionL =
    smosFileCursorSelectedEntryL . entryCursorSelectionL

smosFileCursorSelectPrev :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectPrev =
    forestCursorSelectPrev rebuildEntryCursor makeEntryCursor

smosFileCursorSelectNext :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectNext =
    forestCursorSelectNext rebuildEntryCursor makeEntryCursor

smosFileCursorSelectPrevOnSameLevel :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectPrevOnSameLevel =
    forestCursorSelectPrevOnSameLevel rebuildEntryCursor makeEntryCursor

smosFileCursorSelectNextOnSameLevel :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectNextOnSameLevel =
    forestCursorSelectNextOnSameLevel rebuildEntryCursor makeEntryCursor

smosFileCursorInsertEntryBefore :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBefore = forestCursorInsert emptyEntry

smosFileCursorInsertEntryBeforeAndSelectHeader ::
       SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBeforeAndSelectHeader =
    (smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtStart) .
    fromJust .
    smosFileCursorSelectPrevOnSameLevel . smosFileCursorInsertEntryBefore

smosFileCursorInsertEntryBelow :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelow = forestCursorAddChildToNodeAtStart emptyEntry

smosFileCursorInsertEntryBelowAndSelectHeader ::
       SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelowAndSelectHeader =
    (smosFileCursorEntrySelectionL .~ HeaderSelected) .
    fromJust .
    forestCursorSelectBelowAtStart rebuildEntryCursor makeEntryCursor .
    smosFileCursorInsertEntryBelow

smosFileCursorInsertEntryAfter :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryAfter = forestCursorAppend emptyEntry

smosFileCursorInsertEntryAfterAndSelectHeader ::
       SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryAfterAndSelectHeader =
    (smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtStart) .
    fromJust .
    smosFileCursorSelectNextOnSameLevel . smosFileCursorInsertEntryAfter

smosFileCursorDeleteSubTree :: SmosFileCursor -> DeleteOrUpdate SmosFileCursor
smosFileCursorDeleteSubTree = forestCursorDeleteSubTree makeEntryCursor

smosFileCursorDeleteElem :: SmosFileCursor -> DeleteOrUpdate SmosFileCursor
smosFileCursorDeleteElem = forestCursorDeleteElem makeEntryCursor

smosFileCursorSwapPrev :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSwapPrev = forestCursorSwapPrev

smosFileCursorSwapNext :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSwapNext = forestCursorSwapNext

smosFileCursorPromoteEntry :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorPromoteEntry =
    forestCursorPromoteElem rebuildEntryCursor makeEntryCursor

smosFileCursorPromoteSubTree :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorPromoteSubTree =
    forestCursorPromoteSubTree rebuildEntryCursor makeEntryCursor

smosFileCursorDemoteEntry :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorDemoteEntry =
    forestCursorDemoteElem rebuildEntryCursor makeEntryCursor

smosFileCursorDemoteSubTree :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorDemoteSubTree =
    forestCursorDemoteSubTree rebuildEntryCursor makeEntryCursor
