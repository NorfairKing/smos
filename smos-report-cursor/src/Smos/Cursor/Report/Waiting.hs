{-# LANGUAGE DeriveGeneric #-}

module Smos.Cursor.Report.Waiting where

import Conduit
import Cursor.Simple.Forest
import Cursor.Simple.List.NonEmpty
import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Lens.Micro
import Path
import Smos.Data
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import Smos.Report.Waiting

produceWaitingReportCursor :: DirectoryConfig -> IO WaitingReportCursor
produceWaitingReportCursor dc = do
  wd <- resolveDirWorkflowDir dc
  runConduit
    $ fmap makeWaitingReportCursor
    $ streamSmosFilesFromWorkflowRel HideArchive dc
      .| filterSmosFilesRel
      .| parseSmosFilesRel wd
      .| printShouldPrint DontPrint
      .| waitingReportConduitHelper Nothing
      .| C.map (uncurry makeWaitingEntryCursor)
      .| sinkList

data WaitingReportCursor
  = WaitingReportCursor
      { waitingReportCursorWaitingEntryCursors :: Maybe (NonEmptyCursor WaitingEntryCursor)
      }
  deriving (Show, Eq, Generic)

instance Validity WaitingReportCursor

makeWaitingReportCursor :: [WaitingEntryCursor] -> WaitingReportCursor
makeWaitingReportCursor = WaitingReportCursor . fmap makeNonEmptyCursor . NE.nonEmpty

data WaitingEntryCursor
  = WaitingEntryCursor
      { waitingEntryCursorFilePath :: Path Rel File,
        waitingEntryCursorForestCursor :: ForestCursor Entry
      }
  deriving (Show, Eq, Generic)

instance Validity WaitingEntryCursor

waitingReportCursorNonEmptyCursorL :: Lens' WaitingReportCursor (Maybe (NonEmptyCursor WaitingEntryCursor))
waitingReportCursorNonEmptyCursorL = lens waitingReportCursorWaitingEntryCursors $ \wrc ne -> wrc {waitingReportCursorWaitingEntryCursors = ne}

makeWaitingEntryCursor :: Path Rel File -> ForestCursor Entry -> WaitingEntryCursor
makeWaitingEntryCursor = WaitingEntryCursor
