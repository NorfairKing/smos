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
import Import
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
          (propertyNameSpecificAttr "estimate", fg green),
          (propertyNameSpecificAttr "goal", fg orange),
          (propertyNameSpecificAttr "timewindow", fg magenta),
          (propertyNameSpecificAttr "url", fg green),
          (projectionHeaderAttr, defAttr `withStyle` underline),
          (waitingReportLongWait, fg red),
          (waitingReportMidWait, fg yellow),
          (waitingReportShortWait, fg blue),
          (waitingReportNoWait, fg green),
          (agendaReportRelativeAttr, defAttr `withStyle` bold),
          (agendaReportNowLineAttr, fg magenta `withStyle` bold),
          (relativeTimestampAccentAttr, defAttr `withStyle` bold),
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
fileAttr = "file"

nonSmosFileAttr :: AttrName
nonSmosFileAttr = "non-smos-file"

dirAttr :: AttrName
dirAttr = "dir"

selectedAttr :: AttrName
selectedAttr = "selected"

savedAttr :: AttrName
savedAttr = "saved"

unsavedAttr :: AttrName
unsavedAttr = "unsaved"

keyAttr :: AttrName
keyAttr = "key"

headerAttr :: AttrName
headerAttr = "header"

contentsAttr :: AttrName
contentsAttr = "contents"

todoStateAttr :: AttrName
todoStateAttr = "todostate"

todoStateSpecificAttr :: TodoState -> AttrName
todoStateSpecificAttr tss = fromString $ "todostate-" ++ T.unpack (todoStateText tss)

todoStateHistoryAttr :: AttrName
todoStateHistoryAttr = "todostatehistory"

timestampNameSpecificAttr :: TimestampName -> AttrName
timestampNameSpecificAttr tsn = fromString $ "timestampname-" ++ T.unpack (timestampNameText tsn)

propertyNameSpecificAttr :: PropertyName -> AttrName
propertyNameSpecificAttr pn = fromString $ "propertyname-" ++ T.unpack (propertyNameText pn)

tagAttr :: AttrName
tagAttr = "tag"

tagSpecificAttr :: Tag -> AttrName
tagSpecificAttr t = fromString $ "tag-" ++ T.unpack (tagText t)

projectionHeaderAttr :: AttrName
projectionHeaderAttr = "projection-header"

waitingReportLongWait :: AttrName
waitingReportLongWait = "waiting-report-long-wait"

waitingReportMidWait :: AttrName
waitingReportMidWait = "waiting-report-mid-wait"

waitingReportShortWait :: AttrName
waitingReportShortWait = "waiting-report-short-wait"

waitingReportNoWait :: AttrName
waitingReportNoWait = "waiting-report-no-wait"

agendaReportRelativeAttr :: AttrName
agendaReportRelativeAttr = "agenda-report-relative-time"

agendaReportNowLineAttr :: AttrName
agendaReportNowLineAttr = "agenda-report-now-line"

relativeTimestampAccentAttr :: AttrName
relativeTimestampAccentAttr = "relative-timestamp-accent"

workReportTitleAttr :: AttrName
workReportTitleAttr = "work-report-title"

workReportWarningAttr :: AttrName
workReportWarningAttr = "work-report-warning"

helpNameAttr :: AttrName
helpNameAttr = "helpdescription"

helpKeyCombinationAttr :: AttrName
helpKeyCombinationAttr = "helpkeycombination"

helpDescriptionAttr :: AttrName
helpDescriptionAttr = "helpdescription"

errorAttr :: AttrName
errorAttr = "error"
