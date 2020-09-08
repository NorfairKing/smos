{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Waiting where

import Conduit
import Cursor.Simple.Forest
import Cursor.Simple.List.NonEmpty
import qualified Data.Conduit.Combinators as C
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Time
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Lens.Micro
import Path
import Smos.Data
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Filter
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
      .| C.concatMap (uncurry makeWaitingEntryCursor)
      .| sinkList

data WaitingReportCursor
  = WaitingReportCursor
      { waitingReportCursorWaitingEntryCursors :: Maybe (NonEmptyCursor WaitingEntryCursor)
      }
  deriving (Show, Eq, Generic)

instance Validity WaitingReportCursor where
  validate wrc@WaitingReportCursor {..} =
    mconcat
      [ genericValidate wrc,
        declare "The waiting entries are in order" $
          let es = maybe [] (NE.toList . rebuildNonEmptyCursor) waitingReportCursorWaitingEntryCursors
           in sortOn waitingEntryCursorTimestamp es == es
      ]

waitingReportCursorNonEmptyCursorL :: Lens' WaitingReportCursor (Maybe (NonEmptyCursor WaitingEntryCursor))
waitingReportCursorNonEmptyCursorL = lens waitingReportCursorWaitingEntryCursors $ \wrc ne -> wrc {waitingReportCursorWaitingEntryCursors = ne}

makeWaitingReportCursor :: [WaitingEntryCursor] -> WaitingReportCursor
makeWaitingReportCursor = WaitingReportCursor . fmap makeNonEmptyCursor . NE.nonEmpty . sortOn waitingEntryCursorTimestamp

data WaitingEntryCursor
  = WaitingEntryCursor
      { waitingEntryCursorFilePath :: Path Rel File,
        waitingEntryCursorForestCursor :: ForestCursor Entry,
        waitingEntryCursorTimestamp :: UTCTime
      }
  deriving (Show, Eq, Generic)

instance Validity WaitingEntryCursor where
  validate wec@WaitingEntryCursor {..} =
    mconcat
      [ genericValidate wec,
        declare "The timestamp matches the forest cursor" $
          parseWaitingStateTimestamp (forestCursorCurrent waitingEntryCursorForestCursor) == Just waitingEntryCursorTimestamp
      ]

makeWaitingEntryCursor :: Path Rel File -> ForestCursor Entry -> Maybe WaitingEntryCursor
makeWaitingEntryCursor path fc = do
  timestamp <- parseWaitingStateTimestamp $ forestCursorCurrent fc
  pure $
    WaitingEntryCursor
      { waitingEntryCursorFilePath = path,
        waitingEntryCursorForestCursor = fc,
        waitingEntryCursorTimestamp = timestamp
      }
