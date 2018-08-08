module Smos.Cursor.SmosFile
    ( SmosFileCursor
    , makeSmosFileCursor
    , rebuildSmosFileCursor
    , startSmosFile
    , smosFileCursorSelectedEntryL
    , smosFileCursorEntrySelectionL
    , smosFileCursorSelectPrevTree
    , smosFileCursorSelectNextTree
    , smosFileCursorSelectFirstTree
    , smosFileCursorSelectLastTree
    , smosFileCursorInsertEntryAbove
    , smosFileCursorInsertEntryBelow
    , smosFileCursorRemoveTreeAndSelectPrev
    , smosFileCursorDeleteTreeAndSelectNext
    , smosFileCursorRemoveTree
    , smosFileCursorDeleteTree
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Lens.Micro

import Cursor.Forest
import Cursor.Tree

import Smos.Data.Types

import Smos.Cursor.Entry

type SmosFileCursor = ForestCursor EntryCursor

makeSmosFileCursor :: NonEmpty (Tree Entry) -> SmosFileCursor
makeSmosFileCursor = makeForestCursor . NE.map (fmap makeEntryCursor)

rebuildSmosFileCursor :: SmosFileCursor -> NonEmpty (Tree Entry)
rebuildSmosFileCursor = NE.map (fmap rebuildEntryCursor) . rebuildForestCursor

startSmosFile :: SmosFileCursor
startSmosFile = makeSmosFileCursor $ Node emptyEntry [] :| []

smosFileCursorSelectedEntryL :: Lens' SmosFileCursor EntryCursor
smosFileCursorSelectedEntryL = forestCursorSelectedTreeL . treeCursorCurrentL

smosFileCursorEntrySelectionL :: Lens' SmosFileCursor EntryCursorSelection
smosFileCursorEntrySelectionL =
    smosFileCursorSelectedEntryL . entryCursorSelectionL

smosFileCursorSelectPrevTree :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectPrevTree = forestCursorSelectPrevTree

smosFileCursorSelectNextTree :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectNextTree = forestCursorSelectNextTree

smosFileCursorSelectFirstTree :: SmosFileCursor -> SmosFileCursor
smosFileCursorSelectFirstTree = forestCursorSelectFirstTree

smosFileCursorSelectLastTree :: SmosFileCursor -> SmosFileCursor
smosFileCursorSelectLastTree = forestCursorSelectLastTree

smosFileCursorInsertEntryAbove :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryAbove sfc =
    forestCursorInsert sfc $ singletonTreeCursor emptyEntryCursor

smosFileCursorInsertEntryBelow :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelow sfc =
    forestCursorAppend sfc $ singletonTreeCursor emptyEntryCursor

smosFileCursorRemoveTreeAndSelectPrev :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorRemoveTreeAndSelectPrev = forestCursorRemoveTreeAndSelectPrev

smosFileCursorDeleteTreeAndSelectNext :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorDeleteTreeAndSelectNext = forestCursorDeleteTreeAndSelectNext

smosFileCursorRemoveTree :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorRemoveTree = forestCursorRemoveTree

smosFileCursorDeleteTree :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorDeleteTree = forestCursorDeleteTree
