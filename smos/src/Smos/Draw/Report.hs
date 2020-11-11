{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Draw.Report
  ( drawReportCursor,
  )
where

import Brick.Types as B
import Brick.Widgets.Core as B
import Cursor.Brick
import Data.Time
import Lens.Micro
import Path
import Smos.Actions
import Smos.Data
import Smos.Draw.Base
import Smos.Report.Filter
import Smos.Report.Formatting
import Smos.Style
import Smos.Types

drawReportCursor :: Select -> ReportCursor -> Drawer
drawReportCursor s = \case
  ReportNextActions narc -> pure $ drawNextActionReportCursor s narc
  ReportWaiting wrc -> drawWaitingReportCursor s wrc

drawNextActionReportCursor :: Select -> NextActionReportCursor -> Widget ResourceName
drawNextActionReportCursor s NextActionReportCursor {..} =
  withHeading (str "Next Action Report") $
    vBox
      [ padAll 1
          $ viewport ResourceViewport Vertical
          $ case nextActionReportCursorSelectedNextActionEntryCursors of
            Nothing -> txtWrap "Empty next action report"
            Just naecs -> verticalNonEmptyCursorTable (go NotSelected) (go s) (go NotSelected) naecs,
        ( case nextActionReportCursorSelection of
            NextActionReportFilterSelected -> withAttr selectedAttr
            NextActionReportSelected -> id
        )
          $ let ms =
                  case nextActionReportCursorSelection of
                    NextActionReportFilterSelected -> MaybeSelected
                    NextActionReportSelected -> NotSelected
             in hBox [textLineWidget "Filter:", txt " ", drawTextCursor ms nextActionReportCursorFilterBar]
      ]
  where
    go = drawNextActionEntryCursor

drawNextActionEntryCursor :: Select -> NextActionEntryCursor -> [Widget ResourceName]
drawNextActionEntryCursor s naec@NextActionEntryCursor {..} =
  let e@Entry {..} = naec ^. nextActionEntryCursorEntryL
      sel =
        ( case s of
            MaybeSelected -> forceAttr selectedAttr . visible
            NotSelected -> id
        )
   in [ drawFilePath nextActionEntryCursorFilePath,
        maybe emptyWidget drawTodoState $ entryState e,
        sel $ drawHeader entryHeader
      ]

drawWaitingReportCursor :: Select -> WaitingReportCursor -> Drawer
drawWaitingReportCursor s WaitingReportCursor {..} = do
  now <- asks zonedTimeToUTC
  let go = drawWaitingEntryCursor now
  pure $ withHeading (str "Waiting Report")
    $ padAll 1
    $ viewport ResourceViewport Vertical
    $ case waitingReportCursorWaitingEntryCursors of
      Nothing -> txtWrap "Empty waiting report"
      Just wecs -> verticalNonEmptyCursorTable (go NotSelected) (go s) (go NotSelected) wecs

drawWaitingEntryCursor :: UTCTime -> Select -> WaitingEntryCursor -> [Widget ResourceName]
drawWaitingEntryCursor now s WaitingEntryCursor {..} =
  let sel =
        ( case s of
            MaybeSelected -> forceAttr selectedAttr . visible
            NotSelected -> id
        )
   in [ str $ toFilePath waitingEntryCursorFilePath,
        sel $ drawHeader $ entryHeader $ forestCursorCurrent waitingEntryCursorForestCursor,
        daysSinceWidget 7 now waitingEntryCursorTimestamp
      ]

daysSinceWidget :: Word -> UTCTime -> UTCTime -> Widget n
daysSinceWidget threshold now t = withAttr style $ str $ show i <> " days"
  where
    th1 = fromIntegral threshold :: Int
    th2 = floor ((fromIntegral threshold :: Double) / 3 * 2) :: Int
    th3 = floor ((fromIntegral threshold :: Double) / 3) :: Int
    style
      | i >= th1 = waitingReportLongWait
      | i >= th2 = waitingReportMidWait
      | i >= th3 = waitingReportShortWait
      | otherwise = waitingReportNoWait
    i = daysSince now t
