{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Smos.Cursor.SmosFile
  ( SmosFileCursor (..),
    makeSmosFileCursor,
    makeSmosFileCursorEntirely,
    makeSmosFileCursorFromSimpleForestCursor,
    rebuildSmosFileCursor,
    rebuildSmosFileCursorEntirely,
    startSmosFile,
    smosFileCursorForestCursorL,
    smosFileCursorSelectedCollapseEntryL,
    smosFileCursorSelectedEntryL,
    smosFileCursorEntrySelectionL,
    smosFileCursorReadyForStartup,
    smosFileCursorSelectPrev,
    smosFileCursorSelectNext,
    smosFileCursorSelectPrevOnSameLevel,
    smosFileCursorSelectNextOnSameLevel,
    smosFileCursorSelectFirst,
    smosFileCursorSelectLast,
    smosFileCursorSelectAbove,
    smosFileCursorSelectBelowAtStart,
    smosFileCursorSelectBelowAtEnd,
    smosFileCursorToggleCollapse,
    smosFileCursorToggleCollapseRecursively,
    smosFileCursorToggleCollapseEntireEntry,
    smosFileCursorToggleCollapseEntryLens,
    smosFileCursorInsertEntryBefore,
    smosFileCursorInsertEntryBeforeAndSelect,
    smosFileCursorInsertEntryBeforeAndSelectHeader,
    smosFileCursorInsertEntryBelowAtStart,
    smosFileCursorInsertEntryBelowAtStartAndSelect,
    smosFileCursorInsertEntryBelowAtStartAndSelectHeader,
    smosFileCursorInsertEntryBelowAtEnd,
    smosFileCursorInsertEntryBelowAtEndAndSelect,
    smosFileCursorInsertEntryBelowAtEndAndSelectHeader,
    smosFileCursorInsertEntryAfter,
    smosFileCursorInsertEntryAfterAndSelect,
    smosFileCursorInsertEntryAfterAndSelectHeader,
    smosFileCursorDeleteElem,
    smosFileCursorDeleteSubTree,
    smosFileCursorSwapPrev,
    smosFileCursorSwapNext,
    smosFileCursorPromoteEntry,
    smosFileCursorPromoteSubTree,
    smosFileCursorDemoteEntry,
    smosFileCursorDemoteSubTree,
    smosFileCursorClockOutEverywhere,
    smosFileCursorClockOutEverywhereAndClockInHere,
    smosFileCursorUpdateTime,
    smosFileSubtreeSetTodoState,
    smosFileCursorSelectHeader,
  )
where

import Control.Applicative
import Control.DeepSeq
import Cursor.Forest
import Cursor.Tree
import Cursor.Types
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Smos.Cursor.Collapse
import Smos.Cursor.Entry
import Smos.Cursor.Logbook
import Smos.Cursor.StateHistory
import Smos.Data

type FC = ForestCursor (CollapseEntry EntryCursor) (CollapseEntry Entry)

newtype SmosFileCursor = SmosFileCursor
  { smosFileCursorForestCursor :: FC
  }
  deriving (Show, Eq, Generic)

instance Validity SmosFileCursor

instance NFData SmosFileCursor

smosFileCursorForestCursorL :: Lens' SmosFileCursor FC
smosFileCursorForestCursorL = lens smosFileCursorForestCursor $ \sfc fc -> sfc {smosFileCursorForestCursor = fc}

makeSmosFileCursor :: NonEmpty (Tree Entry) -> SmosFileCursor
makeSmosFileCursor =
  SmosFileCursor
    . makeForestCursor (collapseEntryValueL %~ makeEntryCursor)
    . NE.map (fmap makeCollapseEntry . makeCTree)

makeSmosFileCursorEntirely :: SmosFile -> Maybe SmosFileCursor
makeSmosFileCursorEntirely = fmap makeSmosFileCursor . NE.nonEmpty . smosFileForest

makeSmosFileCursorFromSimpleForestCursor :: ForestCursor Entry Entry -> SmosFileCursor
makeSmosFileCursorFromSimpleForestCursor = SmosFileCursor . mapForestCursor (makeCollapseEntry . makeEntryCursor) makeCollapseEntry

rebuildSmosFileCursor :: SmosFileCursor -> NonEmpty (Tree Entry)
rebuildSmosFileCursor =
  NE.map (rebuildCTree . fmap rebuildCollapseEntry)
    . rebuildForestCursor (collapseEntryValueL %~ rebuildEntryCursor)
    . smosFileCursorForestCursor

rebuildSmosFileCursorEntirely :: SmosFileCursor -> SmosFile
rebuildSmosFileCursorEntirely = makeSmosFile . NE.toList . rebuildSmosFileCursor

startSmosFile :: SmosFileCursor
startSmosFile =
  (smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtStart) $
    makeSmosFileCursor $
      Node emptyEntry []
        :| []

smosFileCursorSelectedCollapseEntryL :: Lens' SmosFileCursor (CollapseEntry EntryCursor)
smosFileCursorSelectedCollapseEntryL = smosFileCursorForestCursorL . forestCursorSelectedTreeL . treeCursorCurrentL

smosFileCursorSelectedEntryL :: Lens' SmosFileCursor EntryCursor
smosFileCursorSelectedEntryL = smosFileCursorSelectedCollapseEntryL . collapseEntryValueL

smosFileCursorEntrySelectionL :: Lens' SmosFileCursor EntryCursorSelection
smosFileCursorEntrySelectionL = smosFileCursorSelectedEntryL . entryCursorSelectionL

smosFileCursorReadyForStartup :: SmosFileCursor -> SmosFileCursor
smosFileCursorReadyForStartup = collapseDone . unclockStarted . goToEnd
  where
    goToEnd :: SmosFileCursor -> SmosFileCursor
    goToEnd sfc =
      case smosFileCursorForestCursorL forestCursorOpenCurrentForest sfc of
        Nothing -> sfc
        Just sfc' ->
          case smosFileCursorSelectNext sfc of
            Nothing -> fromMaybe sfc' $ smosFileCursorSelectBelowAtEnd sfc'
            Just sfc'' -> goToEnd sfc''
    unclockStarted :: SmosFileCursor -> SmosFileCursor
    unclockStarted =
      smosFileCursorForestCursorL
        %~ mapForestCursor
          (mapUnclockStarted (logbookOpen . entryLogbook . rebuildEntryCursor))
          (mapUnclockStarted (logbookOpen . entryLogbook))
      where
        mapUnclockStarted :: (a -> Bool) -> CollapseEntry a -> CollapseEntry a
        mapUnclockStarted func ce =
          if func (collapseEntryValue ce)
            then ce {collapseEntryShowLogbook = True}
            else ce
    collapseDone :: SmosFileCursor -> SmosFileCursor
    collapseDone =
      smosFileCursorForestCursorL
        %~ mapForestCursor
          (mapCollapseDone (entryIsDone . rebuildEntryCursor))
          (mapCollapseDone entryIsDone)
      where
        mapCollapseDone :: (a -> Bool) -> CollapseEntry a -> CollapseEntry a
        mapCollapseDone func ce =
          if func (collapseEntryValue ce)
            then
              ce
                { collapseEntryShowTimestamps = False,
                  collapseEntryShowProperties = False
                }
            else ce

smosFileCursorToggleCollapseEntireEntry :: SmosFileCursor -> SmosFileCursor
smosFileCursorToggleCollapseEntireEntry =
  smosFileCursorSelectedCollapseEntryL
    %~ ( \c ->
           collapseEntrySetShowAll
             (not $ c ^. collapseEntryShowContentsL && c ^. collapseEntryShowHistoryL)
             c
       )

smosFileCursorToggleCollapseEntryLens :: Lens' (CollapseEntry EntryCursor) Bool -> SmosFileCursor -> SmosFileCursor
smosFileCursorToggleCollapseEntryLens field =
  smosFileCursorSelectedCollapseEntryL %~ over field not

smosFileCursorSelectPrev :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectPrev = smosFileCursorForestCursorL $ forestCursorSelectPrev rebuild make

smosFileCursorSelectNext :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectNext = smosFileCursorForestCursorL $ forestCursorSelectNext rebuild make

smosFileCursorSelectPrevOnSameLevel :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectPrevOnSameLevel = smosFileCursorForestCursorL $ forestCursorSelectPrevOnSameLevel rebuild make

smosFileCursorSelectNextOnSameLevel :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectNextOnSameLevel = smosFileCursorForestCursorL $ forestCursorSelectNextOnSameLevel rebuild make

smosFileCursorSelectFirst :: SmosFileCursor -> SmosFileCursor
smosFileCursorSelectFirst = smosFileCursorForestCursorL %~ forestCursorSelectFirst rebuild make

smosFileCursorSelectLast :: SmosFileCursor -> SmosFileCursor
smosFileCursorSelectLast = smosFileCursorForestCursorL %~ forestCursorSelectLast rebuild make

smosFileCursorSelectAbove :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectAbove = smosFileCursorForestCursorL $ forestCursorSelectAbove rebuild make

smosFileCursorSelectBelowAtStart :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectBelowAtStart =
  smosFileCursorForestCursorL (forestCursorSelectBelowAtStart rebuild make)
    . (smosFileCursorForestCursorL %~ (\fc -> fromMaybe fc $ forestCursorOpenCurrentForest fc))

smosFileCursorSelectBelowAtEnd :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSelectBelowAtEnd =
  smosFileCursorForestCursorL (forestCursorSelectBelowAtEnd rebuild make)
    . (smosFileCursorForestCursorL %~ (\fc -> fromMaybe fc $ forestCursorOpenCurrentForest fc))

smosFileCursorToggleCollapse :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorToggleCollapse = smosFileCursorForestCursorL forestCursorToggleCurrentForest

smosFileCursorToggleCollapseRecursively :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorToggleCollapseRecursively = smosFileCursorForestCursorL forestCursorToggleCurrentForestRecursively

smosFileCursorInsertEntryBefore :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBefore = smosFileCursorForestCursorL %~ forestCursorInsert (makeCollapseEntry emptyEntry)

smosFileCursorInsertEntryBeforeAndSelect :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBeforeAndSelect =
  smosFileCursorForestCursorL %~ forestCursorInsertAndSelect rebuild make (makeCollapseEntry emptyEntry)

smosFileCursorInsertEntryBeforeAndSelectHeader :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBeforeAndSelectHeader = smosFileCursorSelectHeader . smosFileCursorInsertEntryBeforeAndSelect

smosFileCursorInsertEntryBelowAtStart :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelowAtStart = smosFileCursorForestCursorL %~ forestCursorAddChildToNodeAtStart (makeCollapseEntry emptyEntry)

smosFileCursorInsertEntryBelowAtStartAndSelect :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelowAtStartAndSelect =
  smosFileCursorForestCursorL %~ forestCursorAddChildToNodeAtStartAndSelect rebuild make (makeCollapseEntry emptyEntry)

smosFileCursorInsertEntryBelowAtStartAndSelectHeader :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelowAtStartAndSelectHeader = smosFileCursorSelectHeader . smosFileCursorInsertEntryBelowAtStartAndSelect

smosFileCursorInsertEntryBelowAtEnd :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelowAtEnd = smosFileCursorForestCursorL %~ forestCursorAddChildToNodeAtEnd (makeCollapseEntry emptyEntry)

smosFileCursorInsertEntryBelowAtEndAndSelect :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelowAtEndAndSelect =
  smosFileCursorForestCursorL %~ forestCursorAddChildToNodeAtEndAndSelect rebuild make (makeCollapseEntry emptyEntry)

smosFileCursorInsertEntryBelowAtEndAndSelectHeader :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryBelowAtEndAndSelectHeader = smosFileCursorSelectHeader . smosFileCursorInsertEntryBelowAtEndAndSelect

smosFileCursorInsertEntryAfter :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryAfter = smosFileCursorForestCursorL %~ forestCursorAppend (makeCollapseEntry emptyEntry)

smosFileCursorInsertEntryAfterAndSelect :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryAfterAndSelect =
  smosFileCursorForestCursorL %~ forestCursorAppendAndSelect rebuild make (makeCollapseEntry emptyEntry)

smosFileCursorInsertEntryAfterAndSelectHeader :: SmosFileCursor -> SmosFileCursor
smosFileCursorInsertEntryAfterAndSelectHeader = smosFileCursorSelectHeader . smosFileCursorInsertEntryAfterAndSelect

smosFileCursorDeleteSubTree :: SmosFileCursor -> DeleteOrUpdate SmosFileCursor
smosFileCursorDeleteSubTree = smosFileCursorForestCursorL $ forestCursorDeleteSubTree make

smosFileCursorDeleteElem :: SmosFileCursor -> DeleteOrUpdate SmosFileCursor
smosFileCursorDeleteElem = smosFileCursorForestCursorL $ forestCursorDeleteElem make

smosFileCursorSwapPrev :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSwapPrev = smosFileCursorForestCursorL forestCursorSwapPrev

smosFileCursorSwapNext :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorSwapNext = smosFileCursorForestCursorL forestCursorSwapNext

smosFileCursorPromoteEntry :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorPromoteEntry = smosFileCursorForestCursorL $ forestCursorPromoteElem rebuild make

smosFileCursorPromoteSubTree :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorPromoteSubTree = smosFileCursorForestCursorL $ forestCursorPromoteSubTree rebuild make

smosFileCursorDemoteEntry :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorDemoteEntry = smosFileCursorForestCursorL $ forestCursorDemoteElem rebuild make

smosFileCursorDemoteSubTree :: SmosFileCursor -> Maybe SmosFileCursor
smosFileCursorDemoteSubTree = smosFileCursorForestCursorL $ forestCursorDemoteSubTree rebuild make

smosFileCursorClockOutEverywhere :: UTCTime -> SmosFileCursor -> SmosFileCursor
smosFileCursorClockOutEverywhere now =
  smosFileCursorForestCursorL %~ mapForestCursor (mapAndUncollapseIfChanged goEC) (mapAndUncollapseIfChanged goE)
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
   in sfc' & (smosFileCursorSelectedEntryL . entryCursorLogbookCursorL)
        %~ (\lbc -> fromMaybe lbc $ logbookCursorClockIn now lbc)
        & (smosFileCursorSelectedCollapseEntryL . collapseEntryShowLogbookL .~ True)

smosFileCursorUpdateTime :: ZonedTime -> SmosFileCursor -> SmosFileCursor
smosFileCursorUpdateTime zt = smosFileCursorSelectedEntryL %~ entryCursorUpdateTime zt

smosFileSubtreeSetTodoState :: UTCTime -> Maybe TodoState -> SmosFileCursor -> SmosFileCursor
smosFileSubtreeSetTodoState now mts = smosFileCursorForestCursorL . forestCursorSelectedTreeL . treeCursorCurrentSubTreeL %~ go
  where
    go ::
      (CollapseEntry EntryCursor, CForest (CollapseEntry Entry)) ->
      (CollapseEntry EntryCursor, CForest (CollapseEntry Entry))
    go (ceeec, cfceec) =
      ( ceeec
          & fmap
            ( entryCursorStateHistoryCursorL
                %~ (\mshc -> stateHistoryCursorModTodoState now (const mts) mshc <|> mshc)
            ),
        goCF cfceec
      )
    goCF :: CForest (CollapseEntry Entry) -> CForest (CollapseEntry Entry)
    goCF cf = openForest $ map goCT $ unpackCForest cf
    goCT :: CTree (CollapseEntry Entry) -> CTree (CollapseEntry Entry)
    goCT (CNode ce cf) = CNode ce' $ goCF cf
      where
        ce' =
          fmap
            ( \e ->
                e
                  { entryStateHistory =
                      let sh = entryStateHistory e
                       in fromMaybe sh $ stateHistorySetState now mts sh
                  }
            )
            ce

smosFileCursorSelectHeader :: SmosFileCursor -> SmosFileCursor
smosFileCursorSelectHeader = smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtStart

rebuild :: CollapseEntry EntryCursor -> CollapseEntry Entry
rebuild = collapseEntryValueL %~ rebuildEntryCursor

make :: CollapseEntry Entry -> CollapseEntry EntryCursor
make = collapseEntryValueL %~ makeEntryCursor
