{-# LANGUAGE OverloadedStrings #-}

module Smos.Style
  ( defaultAttrMap,
    selectedAttr,
    savedAttr,
    unsavedAttr,
    keyAttr,
    fileAttr,
    nonSmosFileAttr,
    dirAttr,
    headerAttr,
    contentsAttr,
    todoStateAttr,
    todoStateSpecificAttr,
    todoStateHistoryAttr,
    timestampNameSpecificAttr,
    propertyNameSpecificAttr,
    tagAttr,
    tagSpecificAttr,
    projectionHeaderAttr,
    waitingReportLongWait,
    waitingReportMidWait,
    waitingReportShortWait,
    waitingReportNoWait,
    agendaReportRelativeAttr,
    agendaReportNowLineAttr,
    relativeTimestampAccentAttr,
    workReportTitleAttr,
    workReportWarningAttr,
    helpNameAttr,
    helpKeyCombinationAttr,
    helpDescriptionAttr,
    errorAttr,

    -- * Re-exports
    applyAttrMappings,
    fg,
    bg,
    module Graphics.Vty.Attributes,
  )
where

import Brick.AttrMap as B
import Brick.Util as B
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import Smos.Data

defaultAttrMap :: s -> AttrMap
defaultAttrMap _ =
  let col = rgbColor :: Int -> Int -> Int -> Color
      orange = col 255 175 0 -- 214, #ffaf00
      brown = col 215 95 0 -- 166, #d75f00
   in applyAttrMappings
        [ (todoStateSpecificAttr "TODO", fg red),
          (todoStateSpecificAttr "NEXT", fg orange),
          (todoStateSpecificAttr "STARTED", fg orange),
          (todoStateSpecificAttr "WAITING", fg blue),
          (todoStateSpecificAttr "READY", fg brown),
          (todoStateSpecificAttr "DONE", fg green),
          (todoStateSpecificAttr "CANCELLED", fg green),
          (todoStateSpecificAttr "FAILED", fg brightRed),
          (timestampNameSpecificAttr "BEGIN", fg brown),
          (timestampNameSpecificAttr "END", fg brown),
          (timestampNameSpecificAttr "SCHEDULED", fg orange),
          (timestampNameSpecificAttr "DEADLINE", fg red),
          (propertyNameSpecificAttr "assignee", fg blue),
          (propertyNameSpecificAttr "brainpower", fg brown),
          (propertyNameSpecificAttr "client", fg green),
          (propertyNameSpecificAttr "email_address", fg yellow),
          (propertyNameSpecificAttr "goal", fg orange),
          (propertyNameSpecificAttr "phone_number", fg yellow),
          (propertyNameSpecificAttr "timewindow", fg magenta),
          (propertyNameSpecificAttr "url", fg green),
          (propertyNameSpecificAttr "waiting_threshold", fg blue),
          (projectionHeaderAttr, currentAttr `withStyle` underline),
          (waitingReportLongWait, fg red),
          (waitingReportMidWait, fg yellow),
          (waitingReportShortWait, fg blue),
          (waitingReportNoWait, fg green),
          (agendaReportRelativeAttr, currentAttr `withStyle` bold),
          (agendaReportNowLineAttr, fg magenta `withStyle` bold),
          (relativeTimestampAccentAttr, currentAttr `withStyle` bold),
          (workReportTitleAttr, fg white `withStyle` underline),
          (workReportWarningAttr, fg red `withStyle` underline),
          (selectedAttr <> tagAttr, fg brightWhite),
          (selectedAttr <> headerAttr, fg brightWhite),
          (fileAttr, fg V.yellow),
          (dirAttr, fg brown),
          (nonSmosFileAttr, fg V.green)
        ]
        $ attrMap
          defAttr
          [ (selectedAttr, fg V.white),
            (savedAttr, fg green),
            (unsavedAttr, fg orange),
            (keyAttr, fg orange),
            (headerAttr, fg V.yellow),
            (helpNameAttr, fg V.yellow),
            (helpKeyCombinationAttr, fg V.blue),
            (helpDescriptionAttr, fg V.yellow),
            (errorAttr, fg V.red)
          ]

fileAttr :: AttrName
fileAttr = attrName "file"

nonSmosFileAttr :: AttrName
nonSmosFileAttr = attrName "non-smos-file"

dirAttr :: AttrName
dirAttr = attrName "dir"

selectedAttr :: AttrName
selectedAttr = attrName "selected"

savedAttr :: AttrName
savedAttr = attrName "saved"

unsavedAttr :: AttrName
unsavedAttr = attrName "unsaved"

keyAttr :: AttrName
keyAttr = attrName "key"

headerAttr :: AttrName
headerAttr = attrName "header"

contentsAttr :: AttrName
contentsAttr = attrName "contents"

todoStateAttr :: AttrName
todoStateAttr = attrName "todostate"

todoStateSpecificAttr :: TodoState -> AttrName
todoStateSpecificAttr tss = attrName $ "todostate-" ++ T.unpack (todoStateText tss)

todoStateHistoryAttr :: AttrName
todoStateHistoryAttr = attrName "todostatehistory"

timestampNameSpecificAttr :: TimestampName -> AttrName
timestampNameSpecificAttr tsn = attrName $ "timestampname-" ++ T.unpack (timestampNameText tsn)

propertyNameSpecificAttr :: PropertyName -> AttrName
propertyNameSpecificAttr pn = attrName $ "propertyname-" ++ T.unpack (propertyNameText pn)

tagAttr :: AttrName
tagAttr = attrName "tag"

tagSpecificAttr :: Tag -> AttrName
tagSpecificAttr t = attrName $ "tag-" ++ T.unpack (tagText t)

projectionHeaderAttr :: AttrName
projectionHeaderAttr = attrName "projection-header"

waitingReportLongWait :: AttrName
waitingReportLongWait = attrName "waiting-report-long-wait"

waitingReportMidWait :: AttrName
waitingReportMidWait = attrName "waiting-report-mid-wait"

waitingReportShortWait :: AttrName
waitingReportShortWait = attrName "waiting-report-short-wait"

waitingReportNoWait :: AttrName
waitingReportNoWait = attrName "waiting-report-no-wait"

agendaReportRelativeAttr :: AttrName
agendaReportRelativeAttr = attrName "agenda-report-relative-time"

agendaReportNowLineAttr :: AttrName
agendaReportNowLineAttr = attrName "agenda-report-now-line"

relativeTimestampAccentAttr :: AttrName
relativeTimestampAccentAttr = attrName "relative-timestamp-accent"

workReportTitleAttr :: AttrName
workReportTitleAttr = attrName "work-report-title"

workReportWarningAttr :: AttrName
workReportWarningAttr = attrName "work-report-warning"

helpNameAttr :: AttrName
helpNameAttr = attrName "helpdescription"

helpKeyCombinationAttr :: AttrName
helpKeyCombinationAttr = attrName "helpkeycombination"

helpDescriptionAttr :: AttrName
helpDescriptionAttr = attrName "helpdescription"

errorAttr :: AttrName
errorAttr = attrName "error"
