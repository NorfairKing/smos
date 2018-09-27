module Smos.Cursor.SmosFile
    ( SmosFileCursor
    , makeSmosFileCursor
    , rebuildSmosFileCursor
    , startSmosFile
    , smosFileCursorSelectedEntryL
    , smosFileCursorEntrySelectionL
    , smosFileCursorToggleHideEntireEntry
    , smosFileCursorSelectPrev
    , smosFileCursorSelectNext
    , smosFileCursorSelectPrevOnSameLevel
    , smosFileCursorSelectNextOnSameLevel
    , smosFileCursorSelectFirst
    , smosFileCursorSelectLast
    , smosFileCursorToggleCollapse
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
    , smosFileCursorClockOutEverywhere
    , smosFileCursorClockOutEverywhereAndClockInHere
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Time

import Lens.Micro

import Cursor.Forest
import Cursor.Tree
import Cursor.Types

import Smos.Data.Types

import Smos.Cursor.Collapse
import Smos.Cursor.Entry
import Smos.Cursor.Logbook

type SmosFileCursor
     = ForestCursor (CollapseEntry EntryCursor) (CollapseEntry Entry)

makeSmosFileCursor :: NonEmpty (Tree Entry) -> SmosFileCursor
makeSmosFileCursor =
    makeForestCursor (collapseEntryValueL %~ makeEntryCursor) .
    NE.map (fmap makeCollapseEntry . makeCTree)

rebuildSmosFileCursor :: SmosFileCursor -> NonEmpty (Tree Entry)
rebuildSmosFileCursor =
    NE.map (rebuildCTree . fmap rebuildCollapseEntry) .
    rebuildForestCursor (collapseEntryValueL %~ rebuildEntryCursor)

startSmosFile :: SmosFileCursor
startSmosFile =
    (smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtStart) $
    makeSmosFileCursor $ Node emptyEntry [] :| []

smosFileCursorSelectedEntireL ::
       Lens' SmosFileCursor (CollapseEntry EntryCursor)
smosFileCursorSelectedEntireL = forestCursorSelectedTreeL . treeCursorCurrentL

smosFileCursorSelectedEntryL :: Lens' SmosFileCursor EntryCursor
smosFileCursorSelectedEntryL =
    smosFileCursorSelectedEntireL . collapseEntryValueL

smosFileCursorEntrySelectionL :: Lens' SmosFileCursor EntryCursorSelection
smosFileCursorEntrySelectionL =
    smosFileCursorSelectedEntryL . entryCursorSelectionL

smosFileCursorToggleHideEntireEntry :: SmosFileCursor -> SmosFileCursor
smosFileCursorToggleHideEntireEntry =
    smosFileCursorSelectedEntireL %~
    (\c ->
         collapseEntrySetShowAll
             (not $
              c ^. collapseEntryShowContentsL && c ^. collapseEntryShowHistoryL)
             c)

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

smosFileCursorSelectFirst :: SmosFileCursor -> SmosFileCursor
smosFileCursorSelectFirst = forestCursorSelectFirst rebuild make

smosFileCursorSelectLast :: SmosFileCursor -> SmosFileCursor
smosFileCursorSelectLast = forestCursorSelectLast rebuild make

smosFileCursorToggleCollapse :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorToggleCollapse = forestCursorToggleCurrentForest

smosFileCursorInsertEntryBefore :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBefore =
    forestCursorInsert (makeCollapseEntry emptyEntry)

smosFileCursorInsertEntryBeforeAndSelectHeader ::
       SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBeforeAndSelectHeader =
    (smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtStart) .
    fromJust .
    smosFileCursorSelectPrevOnSameLevel . smosFileCursorInsertEntryBefore

smosFileCursorInsertEntryBelow :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelow =
    forestCursorAddChildToNodeAtStart (makeCollapseEntry emptyEntry)

smosFileCursorInsertEntryBelowAndSelectHeader ::
       SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelowAndSelectHeader =
    (smosFileCursorEntrySelectionL .~ HeaderSelected) .
    fromJust .
    forestCursorSelectBelowAtStart rebuild make . smosFileCursorInsertEntryBelow

smosFileCursorInsertEntryAfter :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryAfter =
    forestCursorAppend (makeCollapseEntry emptyEntry)

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

smosFileCursorClockOutEverywhere :: UTCTime -> SmosFileCursor -> SmosFileCursor
smosFileCursorClockOutEverywhere now =
    mapForestCursor
        (mapAndUncollapseIfChanged goEC)
        (mapAndUncollapseIfChanged goE)
  where
    mapAndUncollapseIfChanged ::
           Eq a => (a -> a) -> CollapseEntry a -> CollapseEntry a
    mapAndUncollapseIfChanged func ce =
        let ce' = func <$> ce
        in if ce' == ce
               then ce'
               else ce' {collapseEntryShowLogbook = True}
    goEC :: EntryCursor -> EntryCursor
    goEC =
        entryCursorLogbookCursorL %~
        (\lbc -> fromMaybe lbc $ logbookCursorClockOut now lbc)
    goE :: Entry -> Entry
    goE e =
        let lb = entryLogbook e
        in e {entryLogbook = fromMaybe lb $ logbookClockOut now lb}

smosFileCursorClockOutEverywhereAndClockInHere ::
       UTCTime -> SmosFileCursor -> SmosFileCursor
smosFileCursorClockOutEverywhereAndClockInHere now sfc =
    let sfc' = smosFileCursorClockOutEverywhere now sfc
    in sfc' & (smosFileCursorSelectedEntryL . entryCursorLogbookCursorL) %~
       (\lbc -> fromMaybe lbc $ logbookCursorClockIn now lbc)

rebuild :: CollapseEntry EntryCursor -> CollapseEntry Entry
rebuild = collapseEntryValueL %~ rebuildEntryCursor

make :: CollapseEntry Entry -> CollapseEntry EntryCursor
make = collapseEntryValueL %~ makeEntryCursor
