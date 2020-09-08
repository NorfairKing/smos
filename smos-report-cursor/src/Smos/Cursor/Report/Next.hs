{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Next where

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

produceNextActionReportCursor :: DirectoryConfig -> IO NextActionReportCursor
produceNextActionReportCursor dc =
  makeNextActionReportCursor
    <$> produceReportCursorEntries
      ( nextActionConduitHelper Nothing
          .| C.map (uncurry makeNextActionEntryCursor)
      )
      dc

data NextActionReportCursor
  = NextActionReportCursor
      { nextActionReportCursorNextActionEntryCursors :: [NextActionEntryCursor],
        nextActionReportCursorSelectedNextActionEntryCursors :: Maybe (NonEmptyCursor NextActionEntryCursor),
        nextActionReportCursorFilterBar :: TextCursor,
        nextActionReportCursorSelection :: NextActionReportCursorSelection
      }
  deriving (Show, Eq, Generic)

instance Validity NextActionReportCursor

data NextActionReportCursorSelection
  = NextActionReportSelected
  | NextActionReportFilterSelected
  deriving (Show, Eq, Generic)

instance Validity NextActionReportCursorSelection

nextActionReportCursorNextActionEntryCursorsL :: Lens' NextActionReportCursor [NextActionEntryCursor]
nextActionReportCursorNextActionEntryCursorsL =
  lens nextActionReportCursorNextActionEntryCursors (\narc naecs -> narc {nextActionReportCursorNextActionEntryCursors = naecs})

nextActionReportCursorSelectedNextActionEntryCursorsL :: Lens' NextActionReportCursor (Maybe (NonEmptyCursor NextActionEntryCursor))
nextActionReportCursorSelectedNextActionEntryCursorsL =
  lens
    nextActionReportCursorSelectedNextActionEntryCursors
    (\narc necM -> narc {nextActionReportCursorSelectedNextActionEntryCursors = necM})

nextActionReportCursorSelectionL :: Lens' NextActionReportCursor NextActionReportCursorSelection
nextActionReportCursorSelectionL = lens nextActionReportCursorSelection (\narc cs -> narc {nextActionReportCursorSelection = cs})

nextActionReportCursorFilterBarL :: Lens' NextActionReportCursor TextCursor
nextActionReportCursorFilterBarL =
  lens nextActionReportCursorFilterBar $
    \narc@NextActionReportCursor {..} tc ->
      let query = parseEntryFilterRel $ rebuildTextCursor tc
       in case query of
            Left _ ->
              narc
                { nextActionReportCursorFilterBar = tc
                }
            Right ef ->
              let filteredIn =
                    filterNextActionEntryCursors ef
                      . toList
                      $ nextActionReportCursorNextActionEntryCursors
               in narc
                    { nextActionReportCursorFilterBar = tc,
                      nextActionReportCursorSelectedNextActionEntryCursors =
                        makeNENextActionEntryCursor filteredIn
                    }

filterNextActionEntryCursors :: EntryFilterRel -> [NextActionEntryCursor] -> [NextActionEntryCursor]
filterNextActionEntryCursors ef = filter (filterPredicate ef . unwrapNextActionEntryCursor)

makeNextActionReportCursor :: [NextActionEntryCursor] -> NextActionReportCursor
makeNextActionReportCursor naecs =
  NextActionReportCursor
    { nextActionReportCursorNextActionEntryCursors = naecs,
      nextActionReportCursorSelectedNextActionEntryCursors = makeNENextActionEntryCursor naecs,
      nextActionReportCursorFilterBar = emptyTextCursor,
      nextActionReportCursorSelection = NextActionReportSelected
    }

makeNENextActionEntryCursor :: [NextActionEntryCursor] -> Maybe (NonEmptyCursor NextActionEntryCursor)
makeNENextActionEntryCursor = fmap makeNonEmptyCursor . NE.nonEmpty

nextActionReportCursorBuildSmosFileCursor :: Path Abs Dir -> NextActionReportCursor -> Maybe (Path Abs File, SmosFileCursor)
nextActionReportCursorBuildSmosFileCursor pad narc = do
  selected <- nonEmptyCursorCurrent <$> nextActionReportCursorSelectedNextActionEntryCursors narc
  let go :: ForestCursor Entry Entry -> SmosFileCursor
      go = SmosFileCursor . mapForestCursor (makeCollapseEntry . makeEntryCursor) makeCollapseEntry
  pure (pad </> nextActionEntryCursorFilePath selected, go $ nextActionEntryCursorForestCursor selected)

nextActionReportCursorNext :: NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorNext = nextActionReportCursorSelectedNextActionEntryCursorsL $
  \msc ->
    Just
      <$> case msc of
        Nothing -> Nothing
        Just sc -> nonEmptyCursorSelectNext sc

nextActionReportCursorPrev :: NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorPrev = nextActionReportCursorSelectedNextActionEntryCursorsL $ \msc ->
  Just
    <$> case msc of
      Nothing -> Nothing
      Just sc -> nonEmptyCursorSelectPrev sc

nextActionReportCursorFirst :: NextActionReportCursor -> NextActionReportCursor
nextActionReportCursorFirst = nextActionReportCursorSelectedNextActionEntryCursorsL %~ fmap nonEmptyCursorSelectFirst

nextActionReportCursorLast :: NextActionReportCursor -> NextActionReportCursor
nextActionReportCursorLast = nextActionReportCursorSelectedNextActionEntryCursorsL %~ fmap nonEmptyCursorSelectLast

nextActionReportCursorSelectReport :: NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorSelectReport = nextActionReportCursorSelectionL $
  \case
    NextActionReportSelected -> Nothing
    NextActionReportFilterSelected -> Just NextActionReportSelected

nextActionReportCursorSelectFilter :: NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorSelectFilter = nextActionReportCursorSelectionL $
  \case
    NextActionReportFilterSelected -> Nothing
    NextActionReportSelected -> Just NextActionReportFilterSelected

nextActionReportCursorInsert :: Char -> NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorInsert c = nextActionReportCursorFilterBarL $ textCursorInsert c

nextActionReportCursorAppend :: Char -> NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorAppend c = nextActionReportCursorFilterBarL $ textCursorAppend c

nextActionReportCursorRemove :: NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorRemove =
  nextActionReportCursorFilterBarL $
    \tc ->
      case textCursorRemove tc of
        Nothing -> Nothing
        Just Deleted -> Nothing
        Just (Updated narc) -> Just narc

nextActionReportCursorDelete :: NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorDelete =
  nextActionReportCursorFilterBarL $
    \tc ->
      case textCursorDelete tc of
        Nothing -> Nothing
        Just Deleted -> Nothing
        Just (Updated narc) -> Just narc

data NextActionEntryCursor
  = NextActionEntryCursor
      { nextActionEntryCursorFilePath :: Path Rel File,
        nextActionEntryCursorForestCursor :: ForestCursor Entry Entry
      }
  deriving (Show, Eq, Generic)

instance Validity NextActionEntryCursor

unwrapNextActionEntryCursor :: NextActionEntryCursor -> (Path Rel File, ForestCursor Entry Entry)
unwrapNextActionEntryCursor NextActionEntryCursor {..} = (nextActionEntryCursorFilePath, nextActionEntryCursorForestCursor)

makeNextActionEntryCursor :: Path Rel File -> ForestCursor Entry Entry -> NextActionEntryCursor
makeNextActionEntryCursor rp fc =
  NextActionEntryCursor {nextActionEntryCursorFilePath = rp, nextActionEntryCursorForestCursor = fc}

cursorPointsToNextAction :: NextActionEntryCursor -> Bool
cursorPointsToNextAction naec =
  maybe False isNextTodoState . entryState $ naec ^. nextActionEntryCursorEntryL

nextActionEntryCursorForestCursorL :: Lens' NextActionEntryCursor (ForestCursor Entry Entry)
nextActionEntryCursorForestCursorL =
  lens nextActionEntryCursorForestCursor $ \nac fc -> nac {nextActionEntryCursorForestCursor = fc}

nextActionEntryCursorEntryL :: Lens' NextActionEntryCursor Entry
nextActionEntryCursorEntryL =
  nextActionEntryCursorForestCursorL . forestCursorSelectedTreeL . treeCursorCurrentL
