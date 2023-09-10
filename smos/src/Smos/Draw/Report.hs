{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.Draw.Report
  ( drawReportCursor,
  )
where

import Brick.Types as B
import Brick.Widgets.Core as B
import Cursor.Brick
import Cursor.List.NonEmpty (foldNonEmptyCursor)
import Cursor.Simple.List.NonEmpty
import Cursor.Simple.Map
import Data.Foldable
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Path
import Smos.Actions
import Smos.CLI.Formatting
import Smos.Cursor.Report.Entry
import Smos.Data
import Smos.Draw.Base
import Smos.Report.Filter
import Smos.Report.Projection
import Smos.Report.Stuck
import Smos.Report.Time
import Smos.Style
import Smos.Types
import Text.Printf

drawReportCursor :: Select -> ReportCursor -> Drawer
drawReportCursor s = \case
  ReportNextActions narc -> drawNextActionReportCursor s narc
  ReportWaiting wrc -> drawWaitingReportCursor s wrc
  ReportTimestamps tsrc -> drawTimestampsReportCursor s tsrc
  ReportStuck src -> drawStuckReportCursor s src
  ReportWork wrc -> drawWorkReportCursor s wrc

drawNextActionReportCursor :: Select -> NextActionReportCursor -> Drawer
drawNextActionReportCursor s NextActionReportCursor {..} = do
  ercw <- drawEntryReportCursorSimple drawNextActionEntryCursor s nextActionReportCursorEntryReportCursor
  pure $ withHeading (str "Next Action Report") $ padAll 1 ercw

drawNextActionEntryCursor :: Select -> EntryReportEntryCursor (TodoState, UTCTime) -> Drawer' [Widget ResourceName]
drawNextActionEntryCursor s EntryReportEntryCursor {..} =
  pure $
    let sel = withVisibleSelected s . withSelPointer s
        (ts, _) = entryReportEntryCursorVal
     in [ drawFilePathInReport entryReportEntryCursorFilePath,
          drawTodoState ts,
          sel $ drawHeader $ entryHeader $ forestCursorCurrent entryReportEntryCursorForestCursor
        ]

drawWaitingReportCursor :: Select -> WaitingReportCursor -> Drawer
drawWaitingReportCursor s WaitingReportCursor {..} = do
  ercw <-
    drawEntryReportCursorWithHeader
      [ str "file",
        withAttr headerAttr $ str "header",
        withAttr (todoStateSpecificAttr "WAITING") $ str "waiting",
        str "threshold"
      ]
      drawWaitingEntryCursor
      s
      waitingReportCursorEntryReportCursor
  pure $ withHeading (str "Waiting Report") $ padAll 1 ercw

drawWaitingEntryCursor :: Select -> EntryReportEntryCursor (UTCTime, Maybe Time) -> Drawer' [Widget ResourceName]
drawWaitingEntryCursor s EntryReportEntryCursor {..} = do
  now <- asks $ zonedTimeToUTC . drawEnvNow

  let (ts, mThreshold) = entryReportEntryCursorVal
  defaultThreshold <- asks drawEnvWaitingThreshold
  let threshold = fromMaybe defaultThreshold mThreshold

  let sel = withVisibleSelected s . withSelPointer s
  pure
    [ drawFilePathInReport entryReportEntryCursorFilePath,
      sel $ drawHeader $ entryHeader $ forestCursorCurrent entryReportEntryCursorForestCursor,
      daysSinceWidgetWithThreshold threshold now ts,
      maybe (str " ") drawTime mThreshold
    ]

daysSinceWidgetWithThreshold :: Time -> UTCTime -> UTCTime -> Widget n
daysSinceWidgetWithThreshold threshold = daysSinceWidget (floor $ timeNominalDiffTime threshold / nominalDay)

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

drawEntryReportCursorWithHeader ::
  [Widget ResourceName] -> (Select -> EntryReportEntryCursor a -> Drawer' [Widget ResourceName]) -> Select -> EntryReportCursor a -> Drawer
drawEntryReportCursorWithHeader h go = drawEntryReportCursor $ \s mnec ->
  case mnec of
    Nothing -> pure $ txtWrap "Empty report"
    Just wecs ->
      verticalNonEmptyCursorTableWithHeaderM
        (go NotSelected)
        (go s)
        (go NotSelected)
        (map (withDefAttr projectionHeaderAttr) h)
        wecs

drawEntryReportCursorSimple ::
  (Select -> EntryReportEntryCursor a -> Drawer' [Widget ResourceName]) -> Select -> EntryReportCursor a -> Drawer
drawEntryReportCursorSimple go = drawEntryReportCursor $ \s mnec ->
  case mnec of
    Nothing -> pure $ txtWrap "Empty report"
    Just wecs ->
      verticalNonEmptyCursorTableM
        (go NotSelected)
        (go s)
        (go NotSelected)
        wecs

drawEntryReportCursor :: (Select -> Maybe (NonEmptyCursor (EntryReportEntryCursor a)) -> Drawer) -> Select -> EntryReportCursor a -> Drawer
drawEntryReportCursor func s erc = do
  tableW <- drawEntryReportCursorTable func s erc
  pure $
    vBox
      [ ( case s of
            MaybeSelected -> viewport ResourceViewport Vertical
            NotSelected -> id
        )
          tableW,
        drawEntryReportCursorFilter s erc
      ]

drawEntryReportCursorTable :: (Select -> Maybe (NonEmptyCursor (EntryReportEntryCursor a)) -> Drawer) -> Select -> EntryReportCursor a -> Drawer
drawEntryReportCursorTable func s EntryReportCursor {..} = do
  func
    ( case entryReportCursorSelection of
        EntryReportFilterSelected -> NotSelected
        EntryReportSelected -> s
    )
    entryReportCursorSelectedEntryReportEntryCursors

drawEntryReportCursorTableSimple :: (Select -> EntryReportEntryCursor a -> Drawer' [Widget ResourceName]) -> Select -> EntryReportCursor a -> Drawer
drawEntryReportCursorTableSimple go s EntryReportCursor {..} = case entryReportCursorSelectedEntryReportEntryCursors of
  Nothing -> pure $ txtWrap "Empty report"
  Just ercs -> verticalNonEmptyCursorTableM (go NotSelected) (go s) (go NotSelected) ercs

drawEntryReportCursorFilter :: Select -> EntryReportCursor a -> Widget ResourceName
drawEntryReportCursorFilter s EntryReportCursor {..} =
  ( case entryReportCursorSelection of
      EntryReportFilterSelected -> withAttr selectedAttr
      EntryReportSelected -> id
  )
    $ let ms =
            case entryReportCursorSelection of
              EntryReportFilterSelected -> s
              EntryReportSelected -> NotSelected
       in hBox [textLineWidget "Filter:", txt " ", drawTextCursor ms entryReportCursorFilterBar]

drawTimestampsReportCursor :: Select -> TimestampsReportCursor -> Drawer
drawTimestampsReportCursor s TimestampsReportCursor {..} = do
  tsrw <-
    drawEntryReportCursor
      ( \s' mnec ->
          case mnec of
            Nothing -> pure $ txtWrap "Empty timestamps report"
            Just tsecs -> do
              now <- asks drawEnvNow
              ws <- mapM (drawTimestampReportLine s') $ makeTimestampReportLines now tsecs
              pure $ tableWidget ws
      )
      s
      timestampsReportCursorEntryReportCursor
  pure $
    withHeading (str "Agenda Report: Today") $
      padAll 1 tsrw

data TimestampsReportLine
  = ReportSelectedEntryLine !(EntryReportEntryCursor TimestampsEntryCursor)
  | ReportEntryLine !(EntryReportEntryCursor TimestampsEntryCursor)
  | ReportNowLine !LocalTime
  | ReportHourLine !Int

makeTimestampReportLines :: ZonedTime -> NonEmptyCursor (EntryReportEntryCursor TimestampsEntryCursor) -> [TimestampsReportLine]
makeTimestampReportLines now = foldNonEmptyCursor $ \befores current afters ->
  insertReportNowLine now $ insertReportHourLines now $ concat [map ReportEntryLine befores, [ReportSelectedEntryLine current], map ReportEntryLine afters]

drawTimestampReportLine :: Select -> TimestampsReportLine -> Drawer' [Widget ResourceName]
drawTimestampReportLine s = \case
  ReportSelectedEntryLine tsec -> drawTimestampsEntryCursor s tsec
  ReportEntryLine tsec -> drawTimestampsEntryCursor NotSelected tsec
  ReportNowLine now ->
    pure $
      map
        (withAttr agendaReportNowLineAttr)
        [ str $ formatTime defaultTimeLocale "%H:%M:%S" now,
          str "--------",
          str "---",
          str "---",
          str "------------",
          str "---"
        ]
  ReportHourLine i ->
    pure
      [ str (printf "%02d:00   " i),
        empty,
        empty,
        empty,
        empty,
        empty
      ]
  where
    empty = str " "

insertReportHourLines :: ZonedTime -> [TimestampsReportLine] -> [TimestampsReportLine]
insertReportHourLines now = go [8 .. 18]
  where
    ZonedTime lt _ = now
    today = localDay lt
    go hs [] = map ReportHourLine hs
    go [] es = es
    go (h : hs) (e : es) =
      let alt = timestampsReportLineLocalTime now e
          hlt = hourLineLocalTime today h
       in if alt < hlt
            then e : go (h : hs) es
            else ReportHourLine h : go hs (e : es)

timestampsReportLineLocalTime :: ZonedTime -> TimestampsReportLine -> LocalTime
timestampsReportLineLocalTime now = \case
  ReportSelectedEntryLine tec -> timestampLocalTime $ timestampsEntryCursorTimestamp $ entryReportEntryCursorVal tec
  ReportEntryLine tec -> timestampLocalTime $ timestampsEntryCursorTimestamp $ entryReportEntryCursorVal tec
  ReportNowLine lt -> lt
  ReportHourLine h -> hourLineLocalTime (localDay $ zonedTimeToLocalTime now) h

hourLineLocalTime :: Day -> Int -> LocalTime
hourLineLocalTime d h = LocalTime d (TimeOfDay h 0 0)

insertReportNowLine :: ZonedTime -> [TimestampsReportLine] -> [TimestampsReportLine]
insertReportNowLine now = go
  where
    nowL = ReportNowLine $ zonedTimeToLocalTime now
    go = \case
      [] -> [nowL]
      (x : xs) ->
        if isBefore now x
          then nowL : x : xs
          else x : go xs

isBefore :: ZonedTime -> TimestampsReportLine -> Bool
isBefore now after =
  let afterLT = timestampsReportLineLocalTime now after
      nowUTC = zonedTimeToUTC now
   in nowUTC <= localTimeToUTC (zonedTimeZone now) afterLT

drawTimestampsEntryCursor :: Select -> EntryReportEntryCursor TimestampsEntryCursor -> Drawer' [Widget ResourceName]
drawTimestampsEntryCursor s EntryReportEntryCursor {..} = do
  let sel = withVisibleSelected s . withSelPointer s
      e = forestCursorCurrent entryReportEntryCursorForestCursor
  let TimestampsEntryCursor {..} = entryReportEntryCursorVal
  tsw <- drawTimestampPrettyRelative timestampsEntryCursorTimestamp
  let mTime = case timestampsEntryCursorTimestamp of
        TimestampDay _ -> Nothing
        TimestampLocalTime lt -> Just $ localTimeOfDay lt
  pure
    [ maybe (str "     ") (str . formatTime defaultTimeLocale "%H:%M") mTime,
      withAttr agendaReportRelativeAttr tsw,
      drawTimestampName timestampsEntryCursorTimestampName,
      maybe (str " ") drawTodoState $ entryState e,
      sel $ drawHeader $ entryHeader e,
      str $ fromRelFile entryReportEntryCursorFilePath
    ]

drawStuckReportCursor :: Select -> StuckReportCursor -> Drawer
drawStuckReportCursor s StuckReportCursor {..} = do
  sprw <- case stuckReportCursorNonEmptyCursor of
    Nothing -> pure $ str "Empty stuck projects report"
    Just wecs -> verticalNonEmptyCursorTableM (drawStuckReportEntry NotSelected) (drawStuckReportEntry s) (drawStuckReportEntry NotSelected) wecs
  pure $ withHeading (str "Stuck Projects Report") $ padAll 1 sprw

drawStuckReportEntry :: Select -> StuckReportEntry -> Drawer' [Widget ResourceName]
drawStuckReportEntry s StuckReportEntry {..} = do
  now <- asks $ zonedTimeToUTC . drawEnvNow
  threshold <- asks drawEnvStuckThreshold
  let sel = withVisibleSelected s . withSelPointer s
  pure
    [ drawFilePathInReport stuckReportEntryFilePath,
      maybe (str " ") drawTodoState stuckReportEntryState,
      sel $ drawHeader stuckReportEntryHeader,
      maybe
        (str " ")
        (\ts -> if ts > now then str "future" else daysSinceWidgetWithThreshold threshold now ts)
        stuckReportEntryLatestChange
    ]

drawWorkReportCursor :: Select -> WorkReportCursor -> Drawer
drawWorkReportCursor s wrc@WorkReportCursor {..} = do
  let WorkReportCursor _ _ _ _ _ _ _ _ _ _ = undefined
  DrawWorkEnv {..} <- asks drawEnvWorkDrawEnv
  let selectIf :: WorkReportCursorSelection -> Select
      selectIf sel =
        if workReportCursorSelection == sel
          then s
          else NotSelected
  let warningSection title mkW = do
        w <- mkW
        pure $
          vBox
            [ withAttr workReportWarningAttr $ str ("WARNING: " <> title),
              w
            ]
      titleSection :: String -> Drawer' (Widget ResourceName) -> Drawer' (Widget ResourceName)
      titleSection title mkW = do
        w <- mkW
        pure $
          vBox
            [ withAttr workReportTitleAttr $ str title,
              w
            ]
  let sectionGens =
        concat
          [ -- Helpful for debugging:
            -- [titleSection "Selection" $ pure $ str $ show workReportCursorSelection],
            [ titleSection "Next meeting" $
                drawNextMeetingEntryCursor (selectIf NextBeginSelected) erec
              | erec <- maybeToList workReportCursorNextBeginCursor
            ],
            [ warningSection "Entries without context" $
                drawEntryReportCursorTableSimple
                  drawWorkReportResultEntryCursor
                  (selectIf WithoutContextSelected)
                  workReportCursorEntriesWithoutContext
              | not $ workReportWithoutContextEmpty wrc
            ],
            [ let filterSection f w = warningSection ("Check violation for " <> T.unpack (renderFilter f)) w
                  go f erc =
                    filterSection f $
                      drawEntryReportCursorTableSimple
                        drawWorkReportResultEntryCursor
                        NotSelected
                        erc
                  goKVC kvc =
                    let (f, erc) = rebuildKeyValueCursor kvc
                     in filterSection f $
                          drawEntryReportCursorTableSimple
                            drawWorkReportResultEntryCursor
                            (selectIf CheckViolationsSelected)
                            erc
               in verticalMapCursorWidgetM go goKVC go mc
              | mc <- maybeToList workReportCursorCheckViolations
            ],
            [ titleSection "Ongoing" $
                drawEntryReportCursorTableSimple
                  drawWorkReportResultEntryCursor
                  (selectIf OngoingSelected)
                  workReportCursorOngoingEntries
              | not $ workReportOngoingEmpty wrc
            ],
            [ titleSection "Upcoming" $
                drawEntryReportCursorTableSimple
                  drawTimestampsEntryCursor
                  (selectIf DeadlinesSelected)
                  (timestampsReportCursorEntryReportCursor workReportCursorDeadlinesCursor)
              | not $ workReportDeadlinesEmpty wrc
            ],
            [ warningSection "Overdue Waiting Entries" $
                drawEntryReportCursorTableSimple
                  drawWaitingEntryCursor
                  (selectIf WaitingSelected)
                  (waitingReportCursorEntryReportCursor workReportCursorOverdueWaiting)
              | not $ workReportOverdueWaitingEmpty wrc
            ],
            [ warningSection "Overdue Stuck Entries" $
                verticalNonEmptyCursorTableM
                  (drawStuckReportEntry NotSelected)
                  (drawStuckReportEntry (selectIf StuckSelected))
                  (drawStuckReportEntry NotSelected)
                  sres
              | sres <- maybeToList (stuckReportCursorNonEmptyCursor workReportCursorOverdueStuck)
            ],
            [ warningSection "Projects without a next action" $
                pure $
                  verticalNonEmptyCursorTable
                    ((: []) . drawFilePathInReport)
                    ((: []) . withVisibleSelected (selectIf LimboSelected) . drawFilePathInReport) -- TODO selection
                    ((: []) . drawFilePathInReport)
                    nec
              | nec <- maybeToList workReportCursorLimboProjects
            ],
            [ titleSection "Next actions" $ case entryReportCursorSelectedEntryReportEntryCursors workReportCursorResultEntries of
                Nothing -> pure $ str "No results"
                Just recs -> do
                  let go = drawWorkReportResultEntryCursor
                  verticalNonEmptyCursorTableWithHeaderM
                    (go NotSelected)
                    (go (selectIf ResultsSelected))
                    (go NotSelected)
                    (toList (drawProjectionHeaderNE drawWorkEnvProjection))
                    recs
            ]
          ]
  sections <- sequence sectionGens
  pure $
    withHeading (str "Work Report") $
      padAll 1 $
        vBox
          [ viewport ResourceViewport Vertical $
              vBox $
                intersperse (str " ") sections,
            drawEntryReportCursorFilter s workReportCursorResultEntries
          ]

drawNextMeetingEntryCursor :: Select -> EntryReportEntryCursor (TimestampName, Timestamp) -> Drawer
drawNextMeetingEntryCursor s EntryReportEntryCursor {..} = do
  let sel = withVisibleSelected s . withSelPointer s
      e = forestCursorCurrent entryReportEntryCursorForestCursor
      (tsn, ts) = entryReportEntryCursorVal
  tsw <- drawTimestampWithPrettyRelative ts
  pure $
    hBox $
      intersperse
        (str " ")
        [ withAttr relativeTimestampAccentAttr tsw,
          drawTimestampName tsn,
          sel $ drawHeader $ entryHeader e,
          str $ fromRelFile entryReportEntryCursorFilePath
        ]

drawWorkReportResultEntryCursor :: Select -> EntryReportEntryCursor () -> Drawer' [Widget ResourceName]
drawWorkReportResultEntryCursor s erc = do
  DrawWorkEnv {..} <- asks drawEnvWorkDrawEnv
  let sel = withVisibleSelected s
  map sel . toList <$> drawProjecteeNE s (projectEntryReportEntryCursor drawWorkEnvProjection erc)

drawProjectionHeaderNE :: NonEmpty Projection -> NonEmpty (Widget n)
drawProjectionHeaderNE = NE.map drawProjectionHeader

drawProjectionHeader :: Projection -> Widget n
drawProjectionHeader =
  withDefAttr projectionHeaderAttr . \case
    OntoFile -> str "file"
    OntoHeader -> withAttr headerAttr $ str "header"
    OntoProperty pn -> drawPropertyName pn
    OntoTag t -> drawTag t
    OntoState -> withAttr todoStateAttr $ str "state"
    OntoTimestamp tn -> drawTimestampName tn
    OntoAncestor p' -> drawProjectionHeader p'

drawProjecteeNE :: Select -> NonEmpty Projectee -> Drawer' (NonEmpty (Widget ResourceName))
drawProjecteeNE s = traverse (drawProjectee s)

drawProjectee :: Select -> Projectee -> Drawer
drawProjectee s = \case
  FileProjection rf -> pure $ drawFilePathInReport rf
  HeaderProjection h -> pure $ withSelPointer s $ drawHeader h
  StateProjection ms -> pure $ maybe (str " ") drawTodoState ms
  TagProjection mt -> pure $ maybe (str " ") drawTag mt
  PropertyProjection pn mpv -> pure $ maybe (str " ") (drawPropertyValue pn) mpv
  TimestampProjection _ mts -> maybe (pure $ str " ") drawTimestampWithPrettyRelative mts
