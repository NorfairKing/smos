module Smos.Cursor.SmosFile
  ( SmosFileCursor
  , makeSmosFileCursor
  , rebuildSmosFileCursor
  , rebuildSmosFileCursorEntirely
  , startSmosFile
  , smosFileCursorSelectedEntryL
  , smosFileCursorEntrySelectionL
  , smosFileCursorReadyForStartup
  , smosFileCursorToggleHideEntireEntry
  , smosFileCursorSelectPrev
  , smosFileCursorSelectNext
  , smosFileCursorSelectPrevOnSameLevel
  , smosFileCursorSelectNextOnSameLevel
  , smosFileCursorSelectFirst
  , smosFileCursorSelectLast
  , smosFileCursorSelectAbove
  , smosFileCursorSelectBelowAtStart
  , smosFileCursorSelectBelowAtEnd
  , smosFileCursorToggleCollapse
  , smosFileCursorToggleCollapseRecursively
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
  , smosFileCursorUpdateTime
  , smosFileSubtreeSetTodoState
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Time

import Control.Applicative

import Lens.Micro

import Cursor.Forest
import Cursor.Tree
import Cursor.Types

import Smos.Data

import Smos.Cursor.Collapse
import Smos.Cursor.Entry
import Smos.Cursor.Logbook
import Smos.Cursor.StateHistory

type SmosFileCursor = ForestCursor (CollapseEntry EntryCursor) (CollapseEntry Entry)

makeSmosFileCursor :: NonEmpty (Tree Entry) -> SmosFileCursor
makeSmosFileCursor =
  makeForestCursor (collapseEntryValueL %~ makeEntryCursor) .
  NE.map (fmap makeCollapseEntry . makeCTree)

rebuildSmosFileCursor :: SmosFileCursor -> NonEmpty (Tree Entry)
rebuildSmosFileCursor =
  NE.map (rebuildCTree . fmap rebuildCollapseEntry) .
  rebuildForestCursor (collapseEntryValueL %~ rebuildEntryCursor)

rebuildSmosFileCursorEntirely :: SmosFileCursor -> SmosFile
rebuildSmosFileCursorEntirely = SmosFile . NE.toList . rebuildSmosFileCursor

startSmosFile :: SmosFileCursor
startSmosFile =
  (smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtStart) $ makeSmosFileCursor $
  Node emptyEntry [] :|
  []

smosFileCursorSelectedEntireL :: Lens' SmosFileCursor (CollapseEntry EntryCursor)
smosFileCursorSelectedEntireL = forestCursorSelectedTreeL . treeCursorCurrentL

smosFileCursorSelectedEntryL :: Lens' SmosFileCursor EntryCursor
smosFileCursorSelectedEntryL = smosFileCursorSelectedEntireL . collapseEntryValueL

smosFileCursorEntrySelectionL :: Lens' SmosFileCursor EntryCursorSelection
smosFileCursorEntrySelectionL = smosFileCursorSelectedEntryL . entryCursorSelectionL

smosFileCursorReadyForStartup :: SmosFileCursor -> SmosFileCursor
smosFileCursorReadyForStartup = unclockStarted . goToEnd
  where
    goToEnd :: SmosFileCursor -> SmosFileCursor
    goToEnd sfc =
      case forestCursorOpenCurrentForest sfc of
        Nothing -> sfc
        Just sfc' ->
          case smosFileCursorSelectNext sfc of
            Nothing -> fromMaybe sfc' $ smosFileCursorSelectBelowAtEnd sfc'
            Just sfc'' -> goToEnd sfc''
    unclockStarted :: SmosFileCursor -> SmosFileCursor
    unclockStarted =
      mapForestCursor
        (mapUnclockStarted (logbookOpen . entryLogbook . rebuildEntryCursor))
        (mapUnclockStarted (logbookOpen . entryLogbook))
      where
        mapUnclockStarted :: (a -> Bool) -> CollapseEntry a -> CollapseEntry a
        mapUnclockStarted func ce =
          if func (collapseEntryValue ce)
            then ce {collapseEntryShowLogbook = True}
            else ce

smosFileCursorToggleHideEntireEntry :: SmosFileCursor -> SmosFileCursor
smosFileCursorToggleHideEntireEntry =
  smosFileCursorSelectedEntireL %~
  (\c ->
     collapseEntrySetShowAll
       (not $ c ^. collapseEntryShowContentsL && c ^. collapseEntryShowHistoryL)
       c)

smosFileCursorSelectPrev :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectPrev = forestCursorSelectPrev rebuild make

smosFileCursorSelectNext :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectNext = forestCursorSelectNext rebuild make

smosFileCursorSelectPrevOnSameLevel :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectPrevOnSameLevel = forestCursorSelectPrevOnSameLevel rebuild make

smosFileCursorSelectNextOnSameLevel :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectNextOnSameLevel = forestCursorSelectNextOnSameLevel rebuild make

smosFileCursorSelectFirst :: SmosFileCursor -> SmosFileCursor
smosFileCursorSelectFirst = forestCursorSelectFirst rebuild make

smosFileCursorSelectLast :: SmosFileCursor -> SmosFileCursor
smosFileCursorSelectLast = forestCursorSelectLast rebuild make

smosFileCursorSelectAbove :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectAbove = forestCursorSelectAbove rebuild make

smosFileCursorSelectBelowAtStart :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectBelowAtStart sfc =
  forestCursorSelectBelowAtStart rebuild make $ fromMaybe sfc $ forestCursorOpenCurrentForest sfc

smosFileCursorSelectBelowAtEnd :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectBelowAtEnd sfc =
  forestCursorSelectBelowAtEnd rebuild make $ fromMaybe sfc $ forestCursorOpenCurrentForest sfc

smosFileCursorToggleCollapse :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorToggleCollapse = forestCursorToggleCurrentForest

smosFileCursorToggleCollapseRecursively :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorToggleCollapseRecursively = forestCursorToggleCurrentForestRecursively

smosFileCursorInsertEntryBefore :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBefore = forestCursorInsert (makeCollapseEntry emptyEntry)

smosFileCursorInsertEntryBeforeAndSelectHeader :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBeforeAndSelectHeader =
  (smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtStart) . fromJust .
  smosFileCursorSelectPrevOnSameLevel .
  smosFileCursorInsertEntryBefore

smosFileCursorInsertEntryBelow :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelow = forestCursorAddChildToNodeAtStart (makeCollapseEntry emptyEntry)

smosFileCursorInsertEntryBelowAndSelectHeader :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelowAndSelectHeader =
  (smosFileCursorEntrySelectionL .~ HeaderSelected) . fromJust .
  forestCursorSelectBelowAtStart rebuild make .
  smosFileCursorInsertEntryBelow

smosFileCursorInsertEntryAfter :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryAfter = forestCursorAppend (makeCollapseEntry emptyEntry)

smosFileCursorInsertEntryAfterAndSelectHeader :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryAfterAndSelectHeader =
  (smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtStart) . fromJust .
  smosFileCursorSelectNextOnSameLevel .
  smosFileCursorInsertEntryAfter

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
  mapForestCursor (mapAndUncollapseIfChanged goEC) (mapAndUncollapseIfChanged goE)
  where
    mapAndUncollapseIfChanged :: Eq a => (a -> a) -> CollapseEntry a -> CollapseEntry a
    mapAndUncollapseIfChanged func ce =
      let ce' = func <$> ce
       in if ce' == ce
            then ce'
            else ce' {collapseEntryShowLogbook = True}
    goEC :: EntryCursor -> EntryCursor
    goEC = entryCursorLogbookCursorL %~ (\lbc -> fromMaybe lbc $ logbookCursorClockOut now lbc)
    goE :: Entry -> Entry
    goE = entryClockOut now

smosFileCursorClockOutEverywhereAndClockInHere :: UTCTime -> SmosFileCursor -> SmosFileCursor
smosFileCursorClockOutEverywhereAndClockInHere now sfc =
  let sfc' = smosFileCursorClockOutEverywhere now sfc
   in sfc' & (smosFileCursorSelectedEntryL . entryCursorLogbookCursorL) %~
      (\lbc -> fromMaybe lbc $ logbookCursorClockIn now lbc) &
      (smosFileCursorSelectedEntireL . collapseEntryShowLogbookL .~ True)

smosFileCursorUpdateTime :: ZonedTime -> SmosFileCursor -> SmosFileCursor
smosFileCursorUpdateTime zt = smosFileCursorSelectedEntryL %~ entryCursorUpdateTime zt

smosFileSubtreeSetTodoState :: UTCTime -> Maybe TodoState -> SmosFileCursor -> SmosFileCursor
smosFileSubtreeSetTodoState now mts = forestCursorSelectedTreeL . treeCursorCurrentSubTreeL %~ go
  where
    go ::
         (CollapseEntry EntryCursor, CForest (CollapseEntry Entry))
      -> (CollapseEntry EntryCursor, CForest (CollapseEntry Entry))
    go (ceeec, cfceec) =
      ( ceeec &
        fmap
          (entryCursorStateHistoryCursorL %~
           (\mshc -> stateHistoryCursorModTodoState now (const mts) mshc <|> mshc))
      , goCF cfceec)
    goCF :: CForest (CollapseEntry Entry) -> CForest (CollapseEntry Entry)
    goCF cf = openForest $ map goCT $ unpackCForest cf
    goCT :: CTree (CollapseEntry Entry) -> CTree (CollapseEntry Entry)
    goCT (CNode ce cf) = CNode ce' $ goCF cf
      where
        ce' =
          fmap
            (\e ->
               e
                 { entryStateHistory =
                     let sh = entryStateHistory e
                      in fromMaybe sh $ stateHistorySetState now mts sh
                 })
            ce

rebuild :: CollapseEntry EntryCursor -> CollapseEntry Entry
rebuild = collapseEntryValueL %~ rebuildEntryCursor

make :: CollapseEntry Entry -> CollapseEntry EntryCursor
make = collapseEntryValueL %~ makeEntryCursor
