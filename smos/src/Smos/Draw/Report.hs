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
import Cursor.Brick.Text
import Lens.Micro
import Smos.Actions
import Smos.Data
import Smos.Draw.Base
import Smos.Style
import Smos.Types

drawReportCursor :: Select -> ReportCursor -> Widget ResourceName
drawReportCursor s = \case
  ReportNextActions narc -> drawNextActionReportCursor s narc

drawNextActionReportCursor :: Select -> NextActionReportCursor -> Widget ResourceName
drawNextActionReportCursor s NextActionReportCursor {..} =
  withHeading (str "Next Action Report") $
    vBox
      [ padAll 1
          $ viewport ResourceViewport Vertical
          $ case nextActionReportCursorSelectedNextActionEntryCursors of
            Nothing -> txtWrap "Empty next action report."
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
