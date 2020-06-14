{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Cursor.Report.Next where

import Conduit
import Cursor.Forest
import Cursor.Simple.List.NonEmpty
import Cursor.Simple.Tree
import Cursor.Text
import Cursor.Types
import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
import Data.Foldable (toList)
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Path
import Smos.Cursor.Collapse
import Smos.Cursor.Entry
import Smos.Cursor.SmosFile
import Smos.Data
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.Next
import Smos.Report.ShouldPrint
import Smos.Report.Streaming

produceNextActionReportCursor :: DirectoryConfig -> IO (Maybe NextActionReportCursor)
produceNextActionReportCursor dc = do
  wd <- resolveDirWorkflowDir dc
  runConduit
    $ fmap makeNextActionReportCursor
    $ streamSmosFilesFromWorkflowRel HideArchive dc
      .| filterSmosFilesRel
      .| parseSmosFilesRel wd
      .| printShouldPrint DontPrint
      .| nextActionConduitHelper Nothing
      .| C.map (uncurry makeNextActionEntryCursor)
      .| sinkList


data NextActionReportCursor
  = NextActionReportCursor
    { nextActionReportCursorNextActionEntryCursors :: NonEmptyCursor NextActionEntryCursor
    , nextActionReportCursorSelectedNextActionEntryCursors :: Maybe (NonEmptyCursor NextActionEntryCursor)
    , nextActionReportCursorFilterBar :: TextCursor
    , nextActionReportCursorFilterValid :: Bool
    , nextActionReportCursorSelection :: NextActionReportCursorSelection
    }
  deriving (Show, Eq, Generic)

instance Validity NextActionReportCursor

data NextActionReportCursorSelection
  = NextActionReportSelected
  | NextActionReportFilterSelected
  deriving (Show, Eq, Generic)

instance Validity NextActionReportCursorSelection

nextActionReportCursorNextActionEntryCursorsL :: Lens' NextActionReportCursor (NonEmptyCursor NextActionEntryCursor)
nextActionReportCursorNextActionEntryCursorsL =
  lens nextActionReportCursorNextActionEntryCursors (\narc naecs -> narc {nextActionReportCursorNextActionEntryCursors = naecs})

nextActionReportCursorSelectedNextActionEntryCursorsL :: Lens' NextActionReportCursor (Maybe (NonEmptyCursor NextActionEntryCursor))
nextActionReportCursorSelectedNextActionEntryCursorsL = lens nextActionReportCursorSelectedNextActionEntryCursors
  (\narc necM -> narc {nextActionReportCursorSelectedNextActionEntryCursors = necM})

nextActionReportCursorSelectionL :: Lens' NextActionReportCursor NextActionReportCursorSelection
nextActionReportCursorSelectionL = lens nextActionReportCursorSelection (\narc cs -> narc {nextActionReportCursorSelection = cs})


nextActionReportCursorFilterBarL :: Lens' NextActionReportCursor TextCursor
nextActionReportCursorFilterBarL =
  lens nextActionReportCursorFilterBar
  $ \narc@(NextActionReportCursor {..}) tc ->
      let query = parseEntryFilterRel $ rebuildTextCursor tc
      in
        case query of
          Left _ -> narc { nextActionReportCursorFilterBar = tc
                         , nextActionReportCursorSelectedNextActionEntryCursors = Just nextActionReportCursorNextActionEntryCursors
                         , nextActionReportCursorFilterValid = False
                         }
          Right ef ->
            let filteredIn =
                  filterNextActionEntryCursors ef
                  . toList . rebuildNonEmptyCursor $ nextActionReportCursorNextActionEntryCursors
            in
            narc { nextActionReportCursorFilterBar = tc
                 , nextActionReportCursorSelectedNextActionEntryCursors =
                     makeNENextActionEntryCursor filteredIn
                 , nextActionReportCursorFilterValid = True
                 }

filterNextActionEntryCursors :: EntryFilterRel -> [NextActionEntryCursor] -> [NextActionEntryCursor]
filterNextActionEntryCursors ef = filter (filterPredicate ef . unwrapNextActionEntryCursor)

makeNextActionReportCursor :: [NextActionEntryCursor] -> Maybe NextActionReportCursor
makeNextActionReportCursor naecs
  = (\nenec ->
    NextActionReportCursor
    { nextActionReportCursorNextActionEntryCursors = nenec
    , nextActionReportCursorSelectedNextActionEntryCursors = Just nenec
    , nextActionReportCursorFilterBar = emptyTextCursor
    , nextActionReportCursorSelection = NextActionReportSelected
    , nextActionReportCursorFilterValid = False
    })
    <$> makeNENextActionEntryCursor naecs

makeNENextActionEntryCursor :: [NextActionEntryCursor] -> Maybe (NonEmptyCursor NextActionEntryCursor)
makeNENextActionEntryCursor = fmap makeNonEmptyCursor . NE.nonEmpty

nextActionReportCursorBuildSmosFileCursor :: NextActionReportCursor -> SmosFileCursor
nextActionReportCursorBuildSmosFileCursor =
  go . nextActionEntryCursorForestCursor . nonEmptyCursorCurrent . (^. nextActionReportCursorNextActionEntryCursorsL)
  where
    go :: ForestCursor Entry Entry -> SmosFileCursor
    go = SmosFileCursor . mapForestCursor (makeCollapseEntry . makeEntryCursor) makeCollapseEntry

nextActionReportCursorBuildFilePath :: Path Abs Dir -> NextActionReportCursor -> Path Abs File
nextActionReportCursorBuildFilePath pad narc =
  let NextActionEntryCursor {..} = nonEmptyCursorCurrent (narc ^. nextActionReportCursorNextActionEntryCursorsL)
   in pad </> nextActionEntryCursorFilePath

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
  nextActionReportCursorFilterBarL
  $ \tc ->
      case textCursorRemove tc of
        Nothing -> Nothing
        Just Deleted -> Nothing
        Just (Updated narc) -> Just narc

nextActionReportCursorDelete :: NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorDelete =
  nextActionReportCursorFilterBarL
  $ \tc ->
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
