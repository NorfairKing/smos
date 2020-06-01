{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Next where

import Conduit
import Cursor.Forest
import Cursor.Simple.List.NonEmpty
import Cursor.Simple.Tree
import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
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
import Smos.Report.Next
import Smos.Report.Path
import Smos.Report.ShouldPrint
import Smos.Report.Streaming

produceNextActionReportCursor :: SmosReportConfig -> IO (Maybe NextActionReportCursor)
produceNextActionReportCursor src = do
  naes <-
    sourceToList $
      streamSmosFilesFromWorkflow HideArchive (smosReportConfigDirectoryConfig src)
        .| parseSmosFiles
        .| printShouldPrint DontPrint
        .| smosFileCursors
        .| C.map (uncurry makeNextActionEntryCursor)
        .| C.filter cursorPointsToNextAction
  pure $ makeNextActionReportCursor naes

type NextActionReportCursor = NonEmptyCursor NextActionEntryCursor

makeNextActionReportCursor :: [NextActionEntryCursor] -> Maybe NextActionReportCursor
makeNextActionReportCursor = fmap makeNonEmptyCursor . NE.nonEmpty

nextActionReportCursorBuildSmosFileCursor :: NextActionReportCursor -> SmosFileCursor
nextActionReportCursorBuildSmosFileCursor =
  go . nextActionEntryCursorForestCursor . nonEmptyCursorCurrent
  where
    go :: ForestCursor Entry Entry -> SmosFileCursor
    go = SmosFileCursor . mapForestCursor (makeCollapseEntry . makeEntryCursor) makeCollapseEntry

nextActionReportCursorBuildFilePath :: NextActionReportCursor -> Path Abs File
nextActionReportCursorBuildFilePath narc =
  let NextActionEntryCursor {..} = nonEmptyCursorCurrent narc
   in case nextActionEntryCursorFilePath of
        Relative pad prf -> pad </> prf
        Absolute paf -> paf

nextActionReportCursorNext :: NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorNext = nonEmptyCursorSelectNext

nextActionReportCursorPrev :: NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorPrev = nonEmptyCursorSelectPrev

nextActionReportCursorFirst :: NextActionReportCursor -> NextActionReportCursor
nextActionReportCursorFirst = nonEmptyCursorSelectFirst

nextActionReportCursorLast :: NextActionReportCursor -> NextActionReportCursor
nextActionReportCursorLast = nonEmptyCursorSelectLast

data NextActionEntryCursor
  = NextActionEntryCursor
      { nextActionEntryCursorFilePath :: RootedPath,
        nextActionEntryCursorForestCursor :: ForestCursor Entry Entry
      }
  deriving (Show, Eq, Generic)

instance Validity NextActionEntryCursor

makeNextActionEntryCursor :: RootedPath -> ForestCursor Entry Entry -> NextActionEntryCursor
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
