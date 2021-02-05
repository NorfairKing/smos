{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Entry where

import Conduit
import Cursor.Forest
import Cursor.Simple.List.NonEmpty
import Cursor.Simple.Tree
import Cursor.Text
import Cursor.Types
import qualified Data.Conduit.Combinators as C
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Path
import Smos.Cursor.Collapse
import Smos.Cursor.Entry
import Smos.Cursor.Report.Streaming
import Smos.Cursor.SmosFile
import Smos.Data
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.Next

produceEntryReportCursor :: DirectoryConfig -> IO EntryReportCursor
produceEntryReportCursor dc =
  makeEntryReportCursor
    <$> produceReportCursorEntries
      ( nextActionConduitHelper Nothing
          .| C.map (uncurry makeEntryReportEntryCursor)
      )
      dc

data EntryReportCursor = EntryReportCursor
  { entryReportCursorEntryReportEntryCursors :: [EntryReportEntryCursor],
    entryReportCursorSelectedEntryReportEntryCursors :: Maybe (NonEmptyCursor EntryReportEntryCursor),
    entryReportCursorFilterBar :: TextCursor,
    entryReportCursorSelection :: EntryReportCursorSelection
  }
  deriving (Show, Eq, Generic)

instance Validity EntryReportCursor

data EntryReportCursorSelection
  = EntryReportSelected
  | EntryReportFilterSelected
  deriving (Show, Eq, Generic)

instance Validity EntryReportCursorSelection

entryReportCursorEntryReportEntryCursorsL :: Lens' EntryReportCursor [EntryReportEntryCursor]
entryReportCursorEntryReportEntryCursorsL =
  lens entryReportCursorEntryReportEntryCursors (\narc naecs -> narc {entryReportCursorEntryReportEntryCursors = naecs})

entryReportCursorSelectedEntryReportEntryCursorsL :: Lens' EntryReportCursor (Maybe (NonEmptyCursor EntryReportEntryCursor))
entryReportCursorSelectedEntryReportEntryCursorsL =
  lens
    entryReportCursorSelectedEntryReportEntryCursors
    (\narc necM -> narc {entryReportCursorSelectedEntryReportEntryCursors = necM})

entryReportCursorSelectionL :: Lens' EntryReportCursor EntryReportCursorSelection
entryReportCursorSelectionL = lens entryReportCursorSelection (\narc cs -> narc {entryReportCursorSelection = cs})

entryReportCursorFilterBarL :: Lens' EntryReportCursor TextCursor
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

filterEntryReportEntryCursors :: EntryFilterRel -> [EntryReportEntryCursor] -> [EntryReportEntryCursor]
filterEntryReportEntryCursors ef = filter (filterPredicate ef . unwrapEntryReportEntryCursor)

makeEntryReportCursor :: [EntryReportEntryCursor] -> EntryReportCursor
makeEntryReportCursor naecs =
  EntryReportCursor
    { entryReportCursorEntryReportEntryCursors = naecs,
      entryReportCursorSelectedEntryReportEntryCursors = makeNEEntryReportEntryCursor naecs,
      entryReportCursorFilterBar = emptyTextCursor,
      entryReportCursorSelection = EntryReportSelected
    }

makeNEEntryReportEntryCursor :: [EntryReportEntryCursor] -> Maybe (NonEmptyCursor EntryReportEntryCursor)
makeNEEntryReportEntryCursor = fmap makeNonEmptyCursor . NE.nonEmpty

entryReportCursorBuildSmosFileCursor :: Path Abs Dir -> EntryReportCursor -> Maybe (Path Abs File, SmosFileCursor)
entryReportCursorBuildSmosFileCursor pad narc = do
  selected <- nonEmptyCursorCurrent <$> entryReportCursorSelectedEntryReportEntryCursors narc
  let go :: ForestCursor Entry Entry -> SmosFileCursor
      go = SmosFileCursor . mapForestCursor (makeCollapseEntry . makeEntryCursor) makeCollapseEntry
  pure (pad </> entryReportEntryCursorFilePath selected, go $ entryReportEntryCursorForestCursor selected)

entryReportCursorNext :: EntryReportCursor -> Maybe EntryReportCursor
entryReportCursorNext = entryReportCursorSelectedEntryReportEntryCursorsL $ mapM nonEmptyCursorSelectNext

entryReportCursorPrev :: EntryReportCursor -> Maybe EntryReportCursor
entryReportCursorPrev = entryReportCursorSelectedEntryReportEntryCursorsL $ mapM nonEmptyCursorSelectPrev

entryReportCursorFirst :: EntryReportCursor -> EntryReportCursor
entryReportCursorFirst = entryReportCursorSelectedEntryReportEntryCursorsL %~ fmap nonEmptyCursorSelectFirst

entryReportCursorLast :: EntryReportCursor -> EntryReportCursor
entryReportCursorLast = entryReportCursorSelectedEntryReportEntryCursorsL %~ fmap nonEmptyCursorSelectLast

entryReportCursorSelectReport :: EntryReportCursor -> Maybe EntryReportCursor
entryReportCursorSelectReport = entryReportCursorSelectionL $
  \case
    EntryReportSelected -> Nothing
    EntryReportFilterSelected -> Just EntryReportSelected

entryReportCursorSelectFilter :: EntryReportCursor -> Maybe EntryReportCursor
entryReportCursorSelectFilter = entryReportCursorSelectionL $
  \case
    EntryReportFilterSelected -> Nothing
    EntryReportSelected -> Just EntryReportFilterSelected

entryReportCursorInsert :: Char -> EntryReportCursor -> Maybe EntryReportCursor
entryReportCursorInsert c = entryReportCursorFilterBarL $ textCursorInsert c

entryReportCursorAppend :: Char -> EntryReportCursor -> Maybe EntryReportCursor
entryReportCursorAppend c = entryReportCursorFilterBarL $ textCursorAppend c

entryReportCursorRemove :: EntryReportCursor -> Maybe EntryReportCursor
entryReportCursorRemove =
  entryReportCursorFilterBarL $
    \tc ->
      case textCursorRemove tc of
        Nothing -> Nothing
        Just Deleted -> Nothing
        Just (Updated narc) -> Just narc

entryReportCursorDelete :: EntryReportCursor -> Maybe EntryReportCursor
entryReportCursorDelete =
  entryReportCursorFilterBarL $
    \tc ->
      case textCursorDelete tc of
        Nothing -> Nothing
        Just Deleted -> Nothing
        Just (Updated narc) -> Just narc

data EntryReportEntryCursor = EntryReportEntryCursor
  { entryReportEntryCursorFilePath :: Path Rel File,
    entryReportEntryCursorForestCursor :: ForestCursor Entry Entry
  }
  deriving (Show, Eq, Generic)

instance Validity EntryReportEntryCursor

unwrapEntryReportEntryCursor :: EntryReportEntryCursor -> (Path Rel File, ForestCursor Entry Entry)
unwrapEntryReportEntryCursor EntryReportEntryCursor {..} = (entryReportEntryCursorFilePath, entryReportEntryCursorForestCursor)

makeEntryReportEntryCursor :: Path Rel File -> ForestCursor Entry Entry -> EntryReportEntryCursor
makeEntryReportEntryCursor rp fc =
  EntryReportEntryCursor {entryReportEntryCursorFilePath = rp, entryReportEntryCursorForestCursor = fc}

cursorPointsToEntry :: EntryReportEntryCursor -> Bool
cursorPointsToEntry naec =
  maybe False isNextTodoState . entryState $ naec ^. entryReportEntryCursorEntryL

entryReportEntryCursorForestCursorL :: Lens' EntryReportEntryCursor (ForestCursor Entry Entry)
entryReportEntryCursorForestCursorL =
  lens entryReportEntryCursorForestCursor $ \nac fc -> nac {entryReportEntryCursorForestCursor = fc}

entryReportEntryCursorEntryL :: Lens' EntryReportEntryCursor Entry
entryReportEntryCursorEntryL =
  entryReportEntryCursorForestCursorL . forestCursorSelectedTreeL . treeCursorCurrentL
