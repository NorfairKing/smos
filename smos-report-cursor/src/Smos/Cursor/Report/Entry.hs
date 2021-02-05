{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Entry where

import Cursor.Forest
import Cursor.Simple.List.NonEmpty
import Cursor.Simple.Tree
import Cursor.Text
import Cursor.Types
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Path
import Smos.Cursor.Collapse
import Smos.Cursor.Entry
import Smos.Cursor.SmosFile
import Smos.Data
import Smos.Report.Filter

data EntryReportCursor a = EntryReportCursor
  { entryReportCursorEntryReportEntryCursors :: ![EntryReportEntryCursor a],
    entryReportCursorSelectedEntryReportEntryCursors :: !(Maybe (NonEmptyCursor (EntryReportEntryCursor a))),
    entryReportCursorFilterBar :: !TextCursor,
    entryReportCursorSelection :: !EntryReportCursorSelection
  }
  deriving (Show, Eq, Generic)

instance Validity a => Validity (EntryReportCursor a)

data EntryReportCursorSelection
  = EntryReportSelected
  | EntryReportFilterSelected
  deriving (Show, Eq, Generic)

instance Validity EntryReportCursorSelection

entryReportCursorEntryReportEntryCursorsL :: Lens' (EntryReportCursor a) [EntryReportEntryCursor a]
entryReportCursorEntryReportEntryCursorsL =
  lens entryReportCursorEntryReportEntryCursors (\narc naecs -> narc {entryReportCursorEntryReportEntryCursors = naecs})

entryReportCursorSelectedEntryReportEntryCursorsL :: Lens' (EntryReportCursor a) (Maybe (NonEmptyCursor (EntryReportEntryCursor a)))
entryReportCursorSelectedEntryReportEntryCursorsL =
  lens
    entryReportCursorSelectedEntryReportEntryCursors
    (\narc necM -> narc {entryReportCursorSelectedEntryReportEntryCursors = necM})

entryReportCursorSelectionL :: Lens' (EntryReportCursor a) EntryReportCursorSelection
entryReportCursorSelectionL = lens entryReportCursorSelection (\narc cs -> narc {entryReportCursorSelection = cs})

entryReportCursorFilterBarL :: Lens' (EntryReportCursor a) TextCursor
entryReportCursorFilterBarL =
  lens entryReportCursorFilterBar $
    \narc@EntryReportCursor {..} tc ->
      let query = parseEntryFilterRel $ rebuildTextCursor tc
       in case query of
            Left _ ->
              narc
                { entryReportCursorFilterBar = tc
                }
            Right ef ->
              let filteredIn =
                    filterEntryReportEntryCursors ef
                      . toList
                      $ entryReportCursorEntryReportEntryCursors
               in narc
                    { entryReportCursorFilterBar = tc,
                      entryReportCursorSelectedEntryReportEntryCursors =
                        makeNEEntryReportEntryCursor filteredIn
                    }

filterEntryReportEntryCursors :: EntryFilterRel -> [EntryReportEntryCursor a] -> [EntryReportEntryCursor a]
filterEntryReportEntryCursors ef = filter (filterPredicate ef . unwrapEntryReportEntryCursor)

makeEntryReportCursor :: [EntryReportEntryCursor a] -> EntryReportCursor a
makeEntryReportCursor naecs =
  EntryReportCursor
    { entryReportCursorEntryReportEntryCursors = naecs,
      entryReportCursorSelectedEntryReportEntryCursors = makeNEEntryReportEntryCursor naecs,
      entryReportCursorFilterBar = emptyTextCursor,
      entryReportCursorSelection = EntryReportSelected
    }

makeNEEntryReportEntryCursor :: [EntryReportEntryCursor a] -> Maybe (NonEmptyCursor (EntryReportEntryCursor a))
makeNEEntryReportEntryCursor = fmap makeNonEmptyCursor . NE.nonEmpty

entryReportCursorBuildSmosFileCursor :: Path Abs Dir -> EntryReportCursor a -> Maybe (Path Abs File, SmosFileCursor, a)
entryReportCursorBuildSmosFileCursor pad narc = do
  selected <- nonEmptyCursorCurrent <$> entryReportCursorSelectedEntryReportEntryCursors narc
  let go :: ForestCursor Entry Entry -> SmosFileCursor
      go = SmosFileCursor . mapForestCursor (makeCollapseEntry . makeEntryCursor) makeCollapseEntry
  pure (pad </> entryReportEntryCursorFilePath selected, go $ entryReportEntryCursorForestCursor selected, entryReportEntryCursorVal selected)

entryReportCursorNext :: EntryReportCursor a -> Maybe (EntryReportCursor a)
entryReportCursorNext = entryReportCursorSelectedEntryReportEntryCursorsL $ mapM nonEmptyCursorSelectNext

entryReportCursorPrev :: EntryReportCursor a -> Maybe (EntryReportCursor a)
entryReportCursorPrev = entryReportCursorSelectedEntryReportEntryCursorsL $ mapM nonEmptyCursorSelectPrev

entryReportCursorFirst :: EntryReportCursor a -> EntryReportCursor a
entryReportCursorFirst = entryReportCursorSelectedEntryReportEntryCursorsL %~ fmap nonEmptyCursorSelectFirst

entryReportCursorLast :: EntryReportCursor a -> EntryReportCursor a
entryReportCursorLast = entryReportCursorSelectedEntryReportEntryCursorsL %~ fmap nonEmptyCursorSelectLast

entryReportCursorSelectReport :: EntryReportCursor a -> Maybe (EntryReportCursor a)
entryReportCursorSelectReport = entryReportCursorSelectionL $
  \case
    EntryReportSelected -> Nothing
    EntryReportFilterSelected -> Just EntryReportSelected

entryReportCursorSelectFilter :: EntryReportCursor a -> Maybe (EntryReportCursor a)
entryReportCursorSelectFilter = entryReportCursorSelectionL $
  \case
    EntryReportFilterSelected -> Nothing
    EntryReportSelected -> Just EntryReportFilterSelected

entryReportCursorInsert :: Char -> EntryReportCursor a -> Maybe (EntryReportCursor a)
entryReportCursorInsert c = entryReportCursorFilterBarL $ textCursorInsert c

entryReportCursorAppend :: Char -> EntryReportCursor a -> Maybe (EntryReportCursor a)
entryReportCursorAppend c = entryReportCursorFilterBarL $ textCursorAppend c

entryReportCursorRemove :: EntryReportCursor a -> Maybe (EntryReportCursor a)
entryReportCursorRemove =
  entryReportCursorFilterBarL $
    \tc ->
      case textCursorRemove tc of
        Nothing -> Nothing
        Just Deleted -> Nothing
        Just (Updated narc) -> Just narc

entryReportCursorDelete :: EntryReportCursor a -> Maybe (EntryReportCursor a)
entryReportCursorDelete =
  entryReportCursorFilterBarL $
    \tc ->
      case textCursorDelete tc of
        Nothing -> Nothing
        Just Deleted -> Nothing
        Just (Updated narc) -> Just narc

data EntryReportEntryCursor a = EntryReportEntryCursor
  { entryReportEntryCursorFilePath :: !(Path Rel File),
    entryReportEntryCursorForestCursor :: !(ForestCursor Entry Entry),
    entryReportEntryCursorVal :: !a
  }
  deriving (Show, Eq, Generic)

instance Validity a => Validity (EntryReportEntryCursor a)

unwrapEntryReportEntryCursor :: EntryReportEntryCursor a -> (Path Rel File, ForestCursor Entry Entry)
unwrapEntryReportEntryCursor EntryReportEntryCursor {..} =
  (entryReportEntryCursorFilePath, entryReportEntryCursorForestCursor)

makeEntryReportEntryCursor :: Path Rel File -> ForestCursor Entry Entry -> a -> EntryReportEntryCursor a
makeEntryReportEntryCursor rp fc a =
  EntryReportEntryCursor
    { entryReportEntryCursorFilePath = rp,
      entryReportEntryCursorForestCursor = fc,
      entryReportEntryCursorVal = a
    }

entryReportEntryCursorForestCursorL :: Lens' (EntryReportEntryCursor a) (ForestCursor Entry Entry)
entryReportEntryCursorForestCursorL =
  lens entryReportEntryCursorForestCursor $ \nac fc -> nac {entryReportEntryCursorForestCursor = fc}

entryReportEntryCursorEntryL :: Lens' (EntryReportEntryCursor a) Entry
entryReportEntryCursorEntryL =
  entryReportEntryCursorForestCursorL . forestCursorSelectedTreeL . treeCursorCurrentL
