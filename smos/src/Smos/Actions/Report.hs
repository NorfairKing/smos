{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report where

import Smos.Cursor.Report.Next
import Smos.Report.Next

import Smos.Types

import Smos.Actions.Utils

reportNextActions :: Action
reportNextActions =
    Action
        { actionName = "reportNextActions"
        , actionFunc =
              modifyEditorCursorS $ \ec -> do
                  rc <- asks configReportConfig
                  naes <- liftIO $ produceNextActionReport rc
                  pure $
                      case makeNextActionReportCursor naes of
                          Nothing -> ec
                          Just narc ->
                              editorCursorSwitchToNextActionReport narc ec
        , actionDescription = "Next action report"
        }

nextNextAction :: Action
nextNextAction =
    Action
        { actionName = "nextNextAction"
        , actionFunc = modifyNextActionReportCursorM nextActionReportCursorNext
        , actionDescription = "Select the next next action"
        }

prevNextAction :: Action
prevNextAction =
    Action
        { actionName = "prevNextAction"
        , actionFunc = modifyNextActionReportCursorM nextActionReportCursorPrev
        , actionDescription = "Select the previous next action"
        }
