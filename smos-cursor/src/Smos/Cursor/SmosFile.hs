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
    , smosFileCursorRemoveTreeAndSelectPrev
    , smosFileCursorDeleteTreeAndSelectNext
    , smosFileCursorRemoveTree
    , smosFileCursorDeleteTree
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe

import Lens.Micro

import Cursor.Forest
import Cursor.Tree
import Cursor.Types

import Smos.Data.Types

import Smos.Cursor.Entry

type SmosFileCursor = ForestCursor EntryCursor

makeSmosFileCursor :: NonEmpty (Tree Entry) -> SmosFileCursor
makeSmosFileCursor = makeForestCursor . NE.map (fmap makeEntryCursor)

rebuildSmosFileCursor :: SmosFileCursor -> NonEmpty (Tree Entry)
rebuildSmosFileCursor = NE.map (fmap rebuildEntryCursor) . rebuildForestCursor

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
smosFileCursorSelectPrev = forestCursorSelectPrev

smosFileCursorSelectNext :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectNext = forestCursorSelectNext

smosFileCursorSelectPrevOnSameLevel :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectPrevOnSameLevel = forestCursorSelectPrevOnSameLevel

smosFileCursorSelectNextOnSameLevel :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectNextOnSameLevel = forestCursorSelectNextOnSameLevel

smosFileCursorInsertEntryBefore :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBefore = forestCursorInsert emptyEntryCursor

smosFileCursorInsertEntryBeforeAndSelectHeader ::
       SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBeforeAndSelectHeader =
    (smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtStart) .
    fromJust .
    smosFileCursorSelectPrevOnSameLevel . smosFileCursorInsertEntryBefore

smosFileCursorInsertEntryBelow :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelow =
    forestCursorAddChildToNodeAtStart emptyEntryCursor

smosFileCursorInsertEntryBelowAndSelectHeader ::
       SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelowAndSelectHeader =
    (smosFileCursorEntrySelectionL .~ HeaderSelected) .
    fromJust . forestCursorSelectBelow . smosFileCursorInsertEntryBelow

smosFileCursorInsertEntryAfter :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryAfter = forestCursorAppend emptyEntryCursor

smosFileCursorInsertEntryAfterAndSelectHeader ::
       SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryAfterAndSelectHeader =
    (smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtStart) .
    fromJust .
    smosFileCursorSelectNextOnSameLevel . smosFileCursorInsertEntryAfter

smosFileCursorRemoveTreeAndSelectPrev ::
       SmosFileCursor -> Maybe (DeleteOrUpdate SmosFileCursor)
smosFileCursorRemoveTreeAndSelectPrev = forestCursorRemoveTreeAndSelectPrev

smosFileCursorDeleteTreeAndSelectNext ::
       SmosFileCursor -> Maybe (DeleteOrUpdate SmosFileCursor)
smosFileCursorDeleteTreeAndSelectNext = forestCursorDeleteTreeAndSelectNext

smosFileCursorRemoveTree :: SmosFileCursor -> DeleteOrUpdate SmosFileCursor
smosFileCursorRemoveTree = forestCursorRemoveTree

smosFileCursorDeleteTree :: SmosFileCursor -> DeleteOrUpdate SmosFileCursor
smosFileCursorDeleteTree = forestCursorDeleteTree
