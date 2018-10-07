{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Next where

import GHC.Generics (Generic)

import Data.Validity

import qualified Data.List.NonEmpty as NE

import Cursor.Forest
import Cursor.Simple.List.NonEmpty
import Cursor.Simple.Tree

import Conduit
import qualified Data.Conduit.Combinators as C
import Lens.Micro
import Path

import Smos.Data

import Smos.Cursor.Collapse
import Smos.Cursor.Entry
import Smos.Cursor.SmosFile

import Smos.Report.Config
import Smos.Report.Next
import Smos.Report.ShouldPrint
import Smos.Report.Streaming

produceNextActionReportCursor ::
       SmosReportConfig -> IO (Maybe NextActionReportCursor)
produceNextActionReportCursor src = do
    wd <- agendaFileSpecGetWorkDir (smosReportConfigAgendaFileSpec src)
    naes <-
        sourceToList $
        sourceFilesInNonHiddenDirsRecursively wd .| filterSmosFiles .|
        parseSmosFiles wd .|
        printShouldPrint DontPrint .|
        smosFileCursors .|
        C.map (uncurry $ makeNextActionEntryCursor wd) .|
        C.filter cursorPointsToNextAction
    pure $ makeNextActionReportCursor naes

type NextActionReportCursor = NonEmptyCursor NextActionEntryCursor

makeNextActionReportCursor ::
       [NextActionEntryCursor] -> Maybe NextActionReportCursor
makeNextActionReportCursor = fmap makeNonEmptyCursor . NE.nonEmpty

nextActionReportCursorBuildSmosFileCursor ::
       NextActionReportCursor -> SmosFileCursor
nextActionReportCursorBuildSmosFileCursor =
    go . nextActionEntryCursorForestCursor . nonEmptyCursorCurrent
  where
    go :: ForestCursor Entry Entry -> SmosFileCursor
    go = mapForestCursor (makeCollapseEntry . makeEntryCursor) makeCollapseEntry

nextActionReportCursorBuildFilePath :: NextActionReportCursor -> Path Abs File
nextActionReportCursorBuildFilePath narc =
    let NextActionEntryCursor {..} = nonEmptyCursorCurrent narc
     in nextActionEntryCursorDirectory </> nextActionEntryCursorFilePath

nextActionReportCursorNext ::
       NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorNext = nonEmptyCursorSelectNext

nextActionReportCursorPrev ::
       NextActionReportCursor -> Maybe NextActionReportCursor
nextActionReportCursorPrev = nonEmptyCursorSelectPrev

nextActionReportCursorFirst :: NextActionReportCursor -> NextActionReportCursor
nextActionReportCursorFirst = nonEmptyCursorSelectFirst

nextActionReportCursorLast :: NextActionReportCursor -> NextActionReportCursor
nextActionReportCursorLast = nonEmptyCursorSelectLast

data NextActionEntryCursor = NextActionEntryCursor
    { nextActionEntryCursorDirectory :: Path Abs Dir
    , nextActionEntryCursorFilePath :: Path Rel File
    , nextActionEntryCursorForestCursor :: ForestCursor Entry Entry
    } deriving (Show, Eq, Generic)

instance Validity NextActionEntryCursor

makeNextActionEntryCursor ::
       Path Abs Dir
    -> Path Rel File
    -> ForestCursor Entry Entry
    -> NextActionEntryCursor
makeNextActionEntryCursor wd rf fc =
    NextActionEntryCursor
        { nextActionEntryCursorDirectory = wd
        , nextActionEntryCursorFilePath = rf
        , nextActionEntryCursorForestCursor = fc
        }

cursorPointsToNextAction :: NextActionEntryCursor -> Bool
cursorPointsToNextAction naec =
    maybe False isNextTodoState . entryState $
    naec ^. nextActionEntryCursorEntryL

nextActionEntryCursorForestCursorL ::
       Lens' NextActionEntryCursor (ForestCursor Entry Entry)
nextActionEntryCursorForestCursorL =
    lens nextActionEntryCursorForestCursor $ \nac fc ->
        nac {nextActionEntryCursorForestCursor = fc}

nextActionEntryCursorEntryL :: Lens' NextActionEntryCursor Entry
nextActionEntryCursorEntryL =
    nextActionEntryCursorForestCursorL .
    forestCursorSelectedTreeL . treeCursorCurrentL
