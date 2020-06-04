{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Draw
  ( smosDraw,
  )
where

import Brick.Types as B
import Brick.Widgets.Border as B
import Brick.Widgets.Center as B
import Brick.Widgets.Core as B
import Brick.Widgets.Core ((<+>))
import Cursor.Brick.Forest
import Cursor.Brick.List.NonEmpty
import Cursor.Brick.Map
import Cursor.Brick.Map.KeyValue
import Cursor.Brick.Text
import Cursor.Brick.TextField
import Cursor.DirForest
import Cursor.DirForest.Brick
import Cursor.FuzzyLocalTime
import Cursor.List.NonEmpty (NonEmptyCursor)
import Cursor.Map
import Cursor.Simple.List.NonEmpty hiding (NonEmptyCursor)
import Cursor.Text
import Cursor.TextField
import Cursor.Tree hiding (drawTreeCursor)
import Data.FuzzyTime
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Text as T
import Data.Time
import Import hiding ((<+>))
import Lens.Micro
import Smos.Actions
import Smos.Cursor.Collapse
import Smos.Cursor.FileBrowser
import Smos.Cursor.Tag
import Smos.Data
import Smos.Draw.Base
import Smos.History
import Smos.Keys
import Smos.Style
import Smos.Types
import Smos.Undo
import Text.Time.Pretty

smosDraw :: SmosConfig -> SmosState -> [Widget ResourceName]
smosDraw SmosConfig {..} ss@SmosState {..} =
  let helpCursorWidget =
        drawHelpCursor configKeyMap (selectWhen HelpSelected) editorCursorHelpCursor
      withHeading hw w =
        let ts =
              if smosStateUnsavedChanges
                then str " | " <+> withAttr unsavedAttr (str "[+]")
                else emptyWidget
         in vBox
              [hBox [str "──[ ", withAttr selectedAttr hw, ts, str " ]──", vLimit 1 $ fill '─'], w]
      fileCursorWidget =
        withHeading (forceAttr selectedAttr $ drawFilePath smosStateFilePath) $
          maybe
            (drawInfo configKeyMap)
            (drawFileCursor $ selectWhen FileSelected)
            (historyPresent editorCursorFileCursor)
      browserCursorWidget =
        withHeading (str "File Browser") $
          maybe
            (drawInfo configKeyMap)
            (drawFileBrowserCursor (selectWhen BrowserSelected))
            editorCursorBrowserCursor
      reportCursorWidget =
        withHeading (str "Next Action Report") $
          maybe
            (str "empty report")
            (drawReportCursor (selectWhen ReportSelected))
            editorCursorReportCursor
      mainCursorWidget =
        case editorCursorSelection of
          FileSelected -> fileCursorWidget
          BrowserSelected -> browserCursorWidget
          ReportSelected -> reportCursorWidget
          HelpSelected -> helpCursorWidget
      debugWidget = [drawDebug ss | editorCursorDebug]
      baseWidget = [vBox $ mainCursorWidget : debugWidget]
   in baseWidget
  where
    EditorCursor {..} = smosStateCursor
    selectWhen :: EditorSelection -> Select
    selectWhen ecs =
      if ecs == editorCursorSelection
        then MaybeSelected
        else NotSelected
    drawFileCursor :: Select -> SmosFileCursor -> Widget ResourceName
    drawFileCursor s = flip runReader smosStateTime . drawSmosFileCursor s

drawInfo :: KeyMap -> Widget n
drawInfo km =
  withAttr selectedAttr
    $ B.vCenterLayer
    $ vBox
    $ map B.hCenterLayer
    $ [ str "SMOS",
        str " ",
        str "version 0.0.0.0",
        str "by Tom Sydney Kerckhove",
        str "Smos is open source and freely distributable",
        str " ",
        str "Building smos takes time, energy and money.",
        str "Please consider supporting the project.",
        str "https://smos.cs-syd.eu/support.html"
      ]
      ++ case lookupStartingActionInKeymap of
        Nothing -> []
        Just kpt ->
          [ str " ",
            str " ",
            str "If you don't know what to do,",
            hBox
              [ str "activate the ",
                withAttr keyAttr $ txt $ actionNameText $ actionName selectHelp,
                str " command using the ",
                withAttr keyAttr $ txt kpt,
                str " key"
              ],
            str "for an overview of the available commands"
          ]
  where
    lookupStartingActionInKeymap :: Maybe Text
    lookupStartingActionInKeymap =
      let emptyMatchers = fileKeyMapEmptyMatchers $ keyMapFileKeyMap km
          matchAction a = actionName a == actionName selectHelp
          matchActionUsing a = actionUsingName a == actionName selectHelp
          matchKeyMapping =
            \case
              MapVtyExactly kp a ->
                if matchAction a
                  then Just (renderKeyPress kp)
                  else Nothing
              MapCatchAll a ->
                if matchAction a
                  then Just "any"
                  else Nothing
              MapAnyTypeableChar au ->
                if matchActionUsing au
                  then Just "<char>"
                  else Nothing
              MapCombination kp km' -> (renderKeyPress kp <>) <$> matchKeyMapping km'
       in msum $ map matchKeyMapping emptyMatchers

drawHelpCursor :: KeyMap -> Select -> Maybe HelpCursor -> Widget ResourceName
drawHelpCursor km _ Nothing = drawInfo km
drawHelpCursor _ s (Just HelpCursor {..}) =
  centerLayer
    $ ( hBorderWithLabel
          (withAttr selectedAttr $ txt ("[Help page for context: " <> helpCursorTitle <> "]"))
          <=>
      )
    $ hBox
      [ vBox
          [ padAll 1
              $ viewport ResourceViewport Vertical
              $ case helpCursorSelectedKeyHelpCursors of
                Nothing -> txtWrap "No matching keybindings found."
                Just hcs -> verticalNonEmptyCursorTable (go NotSelected) (go s) (go NotSelected) hcs,
            ( case helpCursorSelection of
                HelpCursorSearchSelected -> withAttr selectedAttr
                _ -> id
            )
              $ let ms =
                      case helpCursorSelection of
                        HelpCursorSearchSelected -> MaybeSelected
                        _ -> NotSelected
                 in hBox [textLineWidget "Search:", txt " ", drawTextCursor ms helpCursorSearchBar]
          ],
        vBorder,
        padAll 1 $
          case helpCursorSelectedKeyHelpCursors of
            Nothing -> emptyWidget
            Just hcs ->
              let KeyHelpCursor {..} = nonEmptyCursorCurrent hcs
               in vBox
                    [ txt "Name: "
                        <+> withAttr selectedAttr (textWidget $ actionNameText keyHelpCursorName),
                      txt "Description: ",
                      withAttr helpDescriptionAttr $ txtWrap keyHelpCursorDescription
                    ]
      ]
  where
    go :: Select -> KeyHelpCursor -> [Widget n]
    go s_ KeyHelpCursor {..} =
      let msel =
            ( case s_ of
                MaybeSelected -> forceAttr selectedAttr . visible
                NotSelected -> id
            )
       in [ hBox
              $ intersperse (str ", ")
              $ map (withAttr helpKeyCombinationAttr . drawKeyCombination) keyHelpCursorKeyBinding,
            msel $ withAttr helpNameAttr $ textWidget $ actionNameText keyHelpCursorName
          ]

verticalNonEmptyCursorTable ::
  (b -> [Widget n]) -> (a -> [Widget n]) -> (b -> [Widget n]) -> NonEmptyCursor a b -> Widget n
verticalNonEmptyCursorTable prevFunc curFunc nextFunc =
  nonEmptyCursorWidget (\ps c ns -> drawTable $ map prevFunc ps ++ [curFunc c] ++ map nextFunc ns)

drawTable :: [[Widget n]] -> Widget n
drawTable = hBox . intersperse (str " ") . map vBox . transpose

drawKeyCombination :: KeyCombination -> Widget n
drawKeyCombination = txt . renderKeyCombination

drawHistory :: Seq KeyPress -> Widget n
drawHistory = txtWrap . T.unwords . map renderKeyPress . toList

drawDebug :: SmosState -> Widget n
drawDebug SmosState {..} =
  let DebugInfo {..} = smosStateDebugInfo
   in vBox
        [ hBorderWithLabel (str "[ Debug ]"),
          str "Key history: " <+> drawHistory smosStateKeyHistory,
          str "Last match: " <+> fromMaybe emptyWidget (drawLastMatches debugInfoLastMatches),
          case editorCursorSelection smosStateCursor of
            FileSelected ->
              let h = editorCursorFileCursor smosStateCursor
               in vBox
                    [ str "Undo stack length: " <+> str (show (historyUndoLength h)),
                      str "Redo stack length: " <+> str (show (historyRedoLength h))
                    ]
            BrowserSelected -> case editorCursorBrowserCursor smosStateCursor of
              Nothing -> emptyWidget
              Just fbc ->
                let us = fileBrowserCursorUndoStack fbc
                 in vBox
                      [ str "Undo stack length: " <+> str (show (undoStackUndoLength us)),
                        str "Redo stack length: " <+> str (show (undoStackRedoLength us))
                      ]
            _ -> emptyWidget
        ]

drawLastMatches :: Maybe (NonEmpty ActivationDebug) -> Maybe (Widget n)
drawLastMatches Nothing = Nothing
drawLastMatches (Just ts) = Just $ hBox $ intersperse (str " ") $ map go $ NE.toList ts
  where
    go :: ActivationDebug -> Widget n
    go ActivationDebug {..} =
      vBox
        [ str (show activationDebugPrecedence),
          str (show activationDebugPriority),
          drawHistory activationDebugMatch,
          txt $ actionNameText activationDebugName
        ]

defaultPadding :: Padding
defaultPadding = Pad defaultPaddingAmount

defaultPaddingAmount :: Int
defaultPaddingAmount = 2

drawFileBrowserCursor :: Select -> FileBrowserCursor -> Widget ResourceName
drawFileBrowserCursor s =
  viewport ResourceViewport Vertical
    . maybe emptyScreen (verticalPaddedDirForestCursorWidget sel goFodUnselected defaultPaddingAmount)
    . fileBrowserCursorDirForestCursor
  where
    emptyScreen = str "No files to show."
    sel = case s of
      MaybeSelected -> goFodSelected
      NotSelected -> goFodUnselected
    goFodSelected :: FileOrDir () -> Widget ResourceName
    goFodSelected = forceAttr selectedAttr . visible . (str [pointerChar, ' '] <+>) . goFod
    goFodUnselected :: FileOrDir () -> Widget ResourceName
    goFodUnselected = (str [listerChar, ' '] <+>) . goFod
    goFod :: FileOrDir () -> Widget ResourceName
    goFod fod = case fod of
      FodFile rf () ->
        let extraStyle = case fileExtension rf of
              ".smos" -> id -- TODO maybe also something fancy?
              _ -> forceAttr nonSmosFileAttr
         in extraStyle $ drawFilePath rf
      FodDir rd -> drawDirPath rd

drawReportCursor :: Select -> ReportCursor -> Widget ResourceName
drawReportCursor s rc =
  viewport ResourceViewport Vertical $
    case rc of
      ReportNextActions narc -> drawNextActionReportCursor s narc

drawNextActionReportCursor :: Select -> NextActionReportCursor -> Widget ResourceName
drawNextActionReportCursor s =
  verticalNonEmptyCursorTable
    (drawNextActionEntryCursor NotSelected)
    (drawNextActionEntryCursor s)
    (drawNextActionEntryCursor NotSelected)

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

drawSmosFileCursor :: Select -> SmosFileCursor -> Drawer
drawSmosFileCursor s =
  fmap (viewport ResourceViewport Vertical)
    . verticalForestCursorWidgetM drawEntryCTree (drawSmosTreeCursor s) drawEntryCTree
    . smosFileCursorForestCursor

drawSmosTreeCursor ::
  Select -> TreeCursor (CollapseEntry EntryCursor) (CollapseEntry Entry) -> Drawer
drawSmosTreeCursor s tc = fst <$> traverseTreeCursor wrap cur tc
  where
    cur ::
      CollapseEntry EntryCursor ->
      CForest (CollapseEntry Entry) ->
      Drawer' (Widget ResourceName, (Entry, EntryDrawContext))
    cur ec cf =
      case cf of
        EmptyCForest ->
          let edc = emptyEntryDrawContext
           in (,) <$> drawEntryCursor s TreeIsNotCollapsed edc ec
                <*> pure (rebuildEntryCursor $ collapseEntryValue ec, edc)
        ClosedForest ts ->
          let edc = makeClosedEntryDrawContext ts
           in (,) <$> drawEntryCursor s TreeIsCollapsed edc ec
                <*> pure (rebuildEntryCursor $ collapseEntryValue ec, edc)
        OpenForest ts -> do
          let edc = makeOpenEntryDrawContext ts
          ecw <- drawEntryCursor s TreeIsNotCollapsed edc ec
          etws <- mapM drawEntryCTree $ NE.toList ts
          pure
            ( ecw <=> padLeft defaultPadding (vBox etws),
              (rebuildEntryCursor $ collapseEntryValue ec, edc)
            )
    wrap ::
      [CTree (CollapseEntry Entry)] ->
      CollapseEntry Entry ->
      [CTree (CollapseEntry Entry)] ->
      (Widget ResourceName, (Entry, EntryDrawContext)) ->
      Drawer' (Widget ResourceName, (Entry, EntryDrawContext))
    wrap tsl e tsr (w, (b, edc)) = do
      let edc' = completeEntryDrawContext tsl b edc tsr
      befores <- mapM drawEntryCTree tsl
      ew <- drawEntry TreeIsNotCollapsed edc' e
      afters <- mapM drawEntryCTree tsr
      pure
        ( ew <=> padLeft defaultPadding (vBox $ concat [befores, [w], afters]),
          (collapseEntryValue e, edc')
        )

emptyEntryDrawContext :: EntryDrawContext
emptyEntryDrawContext = EntryDrawContext []

makeClosedEntryDrawContext :: NonEmpty (Tree (CollapseEntry Entry)) -> EntryDrawContext
makeClosedEntryDrawContext = EntryDrawContext . map (fmap collapseEntryValue) . NE.toList

makeOpenEntryDrawContext :: NonEmpty (CTree (CollapseEntry Entry)) -> EntryDrawContext
makeOpenEntryDrawContext =
  EntryDrawContext . map (fmap collapseEntryValue . rebuildCTree) . NE.toList

completeEntryDrawContext ::
  [CTree (CollapseEntry Entry)] ->
  Entry ->
  EntryDrawContext ->
  [CTree (CollapseEntry Entry)] ->
  EntryDrawContext
completeEntryDrawContext lts e (EntryDrawContext f) rts =
  let toTrees = map $ fmap collapseEntryValue . rebuildCTree
   in EntryDrawContext $ concat [toTrees lts, [Node e f], toTrees rts]

newtype EntryDrawContext
  = EntryDrawContext
      { entryDrawContextForest :: Forest Entry
      }
  deriving (Show, Eq)

entryDrawContextDirectChildren :: EntryDrawContext -> Word
entryDrawContextDirectChildren = genericLength . entryDrawContextForest

entryDrawContextLegacy :: EntryDrawContext -> Word
entryDrawContextLegacy = forestSize . entryDrawContextForest
  where
    forestSize :: Forest a -> Word
    forestSize = sum . map treeSize
    treeSize :: Tree a -> Word
    treeSize (Node _ f) = 1 + forestSize f

drawEntryCTree :: CTree (CollapseEntry Entry) -> Drawer
drawEntryCTree (CNode t cf) =
  case cf of
    EmptyCForest -> drawEntry TreeIsNotCollapsed emptyEntryDrawContext t
    ClosedForest ts -> drawEntry TreeIsCollapsed (makeClosedEntryDrawContext ts) t
    OpenForest ts -> do
      let edc = makeOpenEntryDrawContext ts
      ew <- drawEntry TreeIsNotCollapsed edc t
      etws <- mapM drawEntryCTree $ NE.toList ts
      pure $ ew <=> padLeft defaultPadding (vBox etws)

completedForestNumbersWidget :: Maybe TodoState -> EntryDrawContext -> Maybe (Widget t)
completedForestNumbersWidget mts edc =
  let es = goF (entryDrawContextForest edc)
      es'@EntryStats {..} = es <> countTodo mts
   in if es == mempty
        then Nothing
        else
          if es' == mempty || (justOne es' && es /= es)
            then Nothing
            else Just $ str $ bracketed $ concat [show entryStatsDone, "/", show entryStatsTotal]
  where
    countDone :: Maybe TodoState -> Word
    countDone (Just "DONE") = 1
    countDone (Just "CANCELLED") = 1
    countDone (Just "FAILED") = 1
    countDone Nothing = 0
    countDone _ = 0
    countTotal :: Maybe TodoState -> Word
    countTotal m =
      if isJust m
        then 1
        else 0
    countTodo :: Maybe TodoState -> EntryStats
    countTodo m = EntryStats {entryStatsDone = countDone m, entryStatsTotal = countTotal m}
    countEntry :: Entry -> EntryStats
    countEntry = countTodo . entryState
    goT :: Tree Entry -> EntryStats
    goT (Node e f) = countEntry e <> goF f
    goF :: Forest Entry -> EntryStats
    goF = mconcat . map goT

data EntryStats
  = EntryStats
      { entryStatsDone :: !Word,
        entryStatsTotal :: !Word
      }
  deriving (Show, Eq)

instance Semigroup EntryStats where
  es1 <> es2 =
    EntryStats
      { entryStatsDone = entryStatsDone es1 + entryStatsDone es2,
        entryStatsTotal = entryStatsTotal es1 + entryStatsTotal es2
      }

instance Monoid EntryStats where
  mempty = EntryStats 0 0
  mappend = (<>)

justOne :: EntryStats -> Bool
justOne es = entryStatsTotal es <= 1

collapsedForestNumbersWidget :: TreeCollapsing -> EntryDrawContext -> Maybe (Widget t)
collapsedForestNumbersWidget tc edc =
  case tc of
    TreeIsNotCollapsed -> Nothing
    TreeIsCollapsed ->
      let direct = entryDrawContextDirectChildren edc
          legacy = entryDrawContextLegacy edc
       in Just
            $ str
            $ unwords
              [ "+++",
                bracketed $
                  if direct == legacy
                    then show direct
                    else
                      concat
                        [ show $ entryDrawContextDirectChildren edc,
                          "|",
                          show $ entryDrawContextLegacy edc
                        ]
              ]

bracketed :: String -> String
bracketed s = "[" ++ s ++ "]"

data TreeCollapsing
  = TreeIsNotCollapsed
  | TreeIsCollapsed
  deriving (Show, Eq)

drawEntryCursor ::
  Select -> TreeCollapsing -> EntryDrawContext -> CollapseEntry EntryCursor -> Drawer
drawEntryCursor s tc edc e = do
  tscw <- forM entryCursorTimestampsCursor $ drawTimestampsCursor (selectWhen TimestampsSelected)
  lbcw <- drawLogbookCursor (selectWhen LogbookSelected) entryCursorLogbookCursor
  shcw <-
    fmap join
      $ forM entryCursorStateHistoryCursor
      $ drawStateHistoryCursor (selectWhen StateHistorySelected)
  pure
    $ ( case s of
          NotSelected -> id
          MaybeSelected -> visible
      )
    $ vBox
    $ catMaybes
      [ Just
          $ hBox
          $ intersperse (str " ")
          $ concat
            [ [ case s of
                  NotSelected -> str [listerChar]
                  MaybeSelected -> withAttr selectedAttr $ str [pointerChar]
              ],
              maybeToList (entryCursorStateHistoryCursor >>= drawCurrentStateFromCursor),
              [drawHeaderCursor (selectWhen HeaderSelected) entryCursorHeaderCursor],
              maybeToList $ drawTagsCursor (selectWhen TagsSelected) <$> entryCursorTagsCursor,
              [ str "..."
                | let e_ = rebuildEntryCursor ec
                   in or
                        [ not (collapseEntryShowContents e) && isJust (entryContents e_),
                          not (collapseEntryShowHistory e)
                            && not (nullStateHistory $ entryStateHistory e_),
                          not (collapseEntryShowLogbook e) && not (nullLogbook $ entryLogbook e_)
                        ]
              ],
              maybeToList $ completedForestNumbersWidget (entryState $ rebuildEntryCursor ec) edc,
              maybeToList $ collapsedForestNumbersWidget tc edc
            ],
        drawIfM collapseEntryShowContents $
          drawContentsCursor (selectWhen ContentsSelected) <$> entryCursorContentsCursor,
        tscw,
        drawPropertiesCursor (selectWhen PropertiesSelected) <$> entryCursorPropertiesCursor,
        drawIfM collapseEntryShowHistory shcw,
        drawIfM collapseEntryShowLogbook lbcw
      ]
  where
    ec@EntryCursor {..} = collapseEntryValue e
    drawIfM :: (forall e. CollapseEntry e -> Bool) -> Maybe a -> Maybe a
    drawIfM bf mw = mw >>= drawIf bf
    drawIf :: (forall e. CollapseEntry e -> Bool) -> a -> Maybe a
    drawIf bf w =
      if bf e
        then Just w
        else Nothing
    selectWhen :: EntryCursorSelection -> Select
    selectWhen ecs =
      s
        <> ( if ecs == entryCursorSelected
               then MaybeSelected
               else NotSelected
           )

drawEntry :: TreeCollapsing -> EntryDrawContext -> CollapseEntry Entry -> Drawer
drawEntry tc edc e = do
  tsw <- drawTimestamps entryTimestamps
  lbw <- drawLogbook entryLogbook
  shw <- drawStateHistory entryStateHistory
  pure
    $ vBox
    $ catMaybes
      [ Just
          $ hBox
          $ intersperse (str " ")
          $ concat
            [ [str [listerChar]],
              maybeToList (drawCurrentState entryStateHistory),
              [drawHeader entryHeader],
              maybeToList (drawTags entryTags),
              [ str "..."
                | or
                    [ not (collapseEntryShowContents e) && isJust entryContents,
                      not (collapseEntryShowHistory e) && not (nullStateHistory entryStateHistory),
                      not (collapseEntryShowLogbook e) && not (nullLogbook entryLogbook)
                    ]
              ],
              maybeToList $ completedForestNumbersWidget (entryState $ collapseEntryValue e) edc,
              maybeToList $ collapsedForestNumbersWidget tc edc
            ],
        drawIfM collapseEntryShowContents $ drawContents <$> entryContents,
        tsw,
        drawProperties entryProperties,
        drawIfM collapseEntryShowHistory shw,
        drawIfM collapseEntryShowLogbook lbw
      ]
  where
    Entry {..} = collapseEntryValue e
    drawIfM :: (forall e. CollapseEntry e -> Bool) -> Maybe a -> Maybe a
    drawIfM bf mw = mw >>= drawIf bf
    drawIf :: (forall e. CollapseEntry e -> Bool) -> a -> Maybe a
    drawIf bf w =
      if bf e
        then Just w
        else Nothing

drawHeaderCursor :: Select -> HeaderCursor -> Widget ResourceName
drawHeaderCursor s = withAttr headerAttr . drawTextCursor s . headerCursorTextCursor

drawHeader :: Header -> Widget ResourceName
drawHeader = withAttr headerAttr . textLineWidget . headerText

drawCurrentStateFromCursor :: StateHistoryCursor -> Maybe (Widget ResourceName)
drawCurrentStateFromCursor = drawCurrentState . rebuildStateHistoryCursor . Just

drawCurrentState :: StateHistory -> Maybe (Widget ResourceName)
drawCurrentState stateHistory =
  stateHistoryState stateHistory <&> \ts -> withAttr todoStateAttr $ drawTodoState ts

drawContentsCursor :: Select -> ContentsCursor -> Widget ResourceName
drawContentsCursor s = drawTextFieldCursor s . contentsCursorTextFieldCursor

drawContents :: Contents -> Widget ResourceName
drawContents = textWidget . contentsText

drawTimestampsCursor :: Select -> TimestampsCursor -> Drawer
drawTimestampsCursor s =
  verticalMapCursorWidgetM drawTimestampPair (drawTimestampKVCursor s) drawTimestampPair
    . timestampsCursorMapCursor

drawTimestamps :: Map TimestampName Timestamp -> MDrawer
drawTimestamps m
  | M.null m = pure Nothing
  | otherwise = Just . vBox <$> mapM (uncurry drawTimestampPair) (M.toList m)

drawTimestampKVCursor ::
  Select -> KeyValueCursor TextCursor FuzzyLocalTimeCursor TimestampName Timestamp -> Drawer
drawTimestampKVCursor s = keyValueWidgetM goKey goVal
  where
    goKey tc ts = do
      dw <- drawTimestamp ts
      pure $
        hBox
          [ case s of
              NotSelected -> drawTimestampName $ rebuildTimestampNameCursor tc
              MaybeSelected -> drawTextCursor s tc,
            str ": ",
            dw
          ]
    goVal tsn fdc = do
      tsw <-
        case s of
          NotSelected -> drawTimestamp $ rebuildTimestampCursor fdc
          MaybeSelected -> drawFuzzyLocalTimeCursor s fdc
      pure $ hBox [drawTimestampName tsn, str ": ", tsw]

drawTimestampPair :: TimestampName -> Timestamp -> Drawer
drawTimestampPair tsn ts = do
  dw <- drawTimestamp ts
  pure $ hBox [drawTimestampName tsn, str ": ", dw]

drawFuzzyLocalTimeCursor :: Select -> FuzzyLocalTimeCursor -> Drawer
drawFuzzyLocalTimeCursor s fdc@FuzzyLocalTimeCursor {..} = do
  dw <-
    case rebuildFuzzyLocalTimeCursor fdc of
      OnlyDaySpecified d -> drawDay d
      BothTimeAndDay lt -> drawLocalTime lt
  pure
    $ ( case s of
          NotSelected -> id
          MaybeSelected -> withAttr selectedAttr
      )
    $ hBox
    $ intersperse (str " ")
    $ drawTextCursor s fuzzyLocalTimeCursorTextCursor
      : [hBox [str "(", dw, str ")"] | MaybeSelected <- [s]]

drawTimestampName :: TimestampName -> Widget n
drawTimestampName tsn =
  withAttr (timestampNameSpecificAttr tsn <> timestampNameAttr) . textLineWidget $
    timestampNameText tsn

drawTimestamp :: Timestamp -> Drawer
drawTimestamp ts =
  case ts of
    TimestampDay d -> drawDay d
    TimestampLocalTime lt -> drawLocalTime lt

drawDay :: Day -> Drawer
drawDay d = do
  zt <- ask
  pure $
    hBox
      [ str $ formatTimestampDay d,
        str ", ",
        str $ prettyDayAuto (localDay $ zonedTimeToLocalTime zt) d
      ]

drawLocalTime :: LocalTime -> Drawer
drawLocalTime lt = do
  zt@(ZonedTime _ tz) <- ask
  pure $
    hBox
      [ str $ formatTimestampLocalTime lt,
        str ", ",
        str $ prettyTimeAuto (zonedTimeToUTC zt) $ localTimeToUTC tz lt
      ]

drawPropertiesCursor :: Select -> PropertiesCursor -> Widget ResourceName
drawPropertiesCursor s =
  verticalMapCursorWidget drawPropertyPair (drawPropertyKVCursor s) drawPropertyPair
    . propertiesCursorMapCursor

drawPropertyKVCursor ::
  Select ->
  KeyValueCursor TextCursor TextCursor PropertyName PropertyValue ->
  Widget ResourceName
drawPropertyKVCursor s = keyValueWidget goKey goVal
  where
    sel =
      case s of
        NotSelected -> id
        MaybeSelected -> withAttr selectedAttr
    goKey tc pv =
      withAttr
        ( maybe
            id
            (\pn -> (<>) (propertyNameSpecificAttr pn))
            (propertyName $ rebuildTextCursor tc)
            propertyNameAttr
        )
        $ hBox [sel $ drawTextCursor s tc, str ": ", drawPropertyValue pv]
    goVal pn tc =
      withAttr (propertyNameSpecificAttr pn <> propertyNameAttr) $
        hBox [drawPropertyName pn, str ": ", sel $ drawTextCursor s tc]

drawProperties :: Map PropertyName PropertyValue -> Maybe (Widget ResourceName)
drawProperties m
  | M.null m = Nothing
  | otherwise = Just $ vBox $ map (uncurry drawPropertyPair) $ M.toList m

drawPropertyPair :: PropertyName -> PropertyValue -> Widget ResourceName
drawPropertyPair pn pv =
  withAttr (propertyNameSpecificAttr pn <> propertyNameAttr) $
    hBox [drawPropertyName pn, str ": ", drawPropertyValue pv]

drawPropertyName :: PropertyName -> Widget ResourceName
drawPropertyName = textLineWidget . propertyNameText

drawPropertyValue :: PropertyValue -> Widget ResourceName
drawPropertyValue = textWidget . propertyValueText

drawStateHistoryCursor :: Select -> StateHistoryCursor -> MDrawer
drawStateHistoryCursor _ = drawStateHistory . rebuildStateHistoryCursor . Just

drawStateHistory :: StateHistory -> MDrawer
drawStateHistory (StateHistory ls)
  | null ls = pure Nothing
  | otherwise = do
    zt <- ask
    pure
      $ Just
      $ withAttr todoStateHistoryAttr
      $ vBox
      $ flip map ls
      $ \StateHistoryEntry {..} ->
        hBox $
          catMaybes
            [ Just
                $ strWrap
                $ unwords
                  [ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" stateHistoryEntryTimestamp,
                    "(" ++ prettyTimeAuto (zonedTimeToUTC zt) stateHistoryEntryTimestamp ++ ")"
                  ],
              (str " " <+>) . drawTodoState <$> stateHistoryEntryNewState
            ]

drawTagsCursor :: Select -> TagsCursor -> Widget ResourceName
drawTagsCursor s =
  horizontalNonEmptyCursorWidget
    (\t -> str ":" <+> drawTag t)
    (drawTagCursor s)
    (\t -> drawTag t <+> str ":")
    . tagsCursorNonEmptyCursor

drawTags :: Set Tag -> Maybe (Widget ResourceName)
drawTags ts
  | S.null ts = Nothing
  | otherwise =
    Just $ str ":" <+> hBox (intersperse (str ":") (map drawTag $ S.toList ts)) <+> str ":"

drawTagCursor :: Select -> TagCursor -> Widget ResourceName
drawTagCursor s =
  ( case s of
      NotSelected -> id
      MaybeSelected -> withAttr selectedAttr
  )
    . (str ":" <+>)
    . (<+> str ":")
    . drawTextCursor s
    . tagCursorTextCursor

drawTag :: Tag -> Widget n
drawTag = textLineWidget . tagText

drawLogbookCursor :: Select -> LogbookCursor -> MDrawer
drawLogbookCursor _ lbc =
  case lbc of
    LogbookCursorClosed Nothing -> pure Nothing
    LogbookCursorClosed (Just ne) -> do
      let lbes = NE.toList $ rebuildNonEmptyCursor ne
      md <- drawLogbookEntries lbes
      tw <- drawLogbookTotal Nothing lbes
      pure $ Just $ vBox $ fromMaybe emptyWidget md : maybeToList tw -- TODO don't use empty widgets
    LogbookCursorOpen u ne -> do
      ow <- drawLogOpen u
      let lbes = maybe [] (NE.toList . rebuildNonEmptyCursor) ne
      md <- drawLogbookEntries lbes
      tw <- drawLogbookTotal (Just u) lbes
      pure $ Just $ vBox $ [ow, fromMaybe emptyWidget md] ++ maybeToList tw

drawLogbook :: Logbook -> MDrawer
drawLogbook (LogClosed ls) = do
  md <- drawLogbookEntries ls
  tw <- drawLogbookTotal Nothing ls
  pure $ Just $ vBox $ maybeToList md ++ maybeToList tw
drawLogbook (LogOpen u ls) = do
  ow <- drawLogOpen u
  md <- drawLogbookEntries ls
  tw <- drawLogbookTotal (Just u) ls
  pure $ Just $ vBox $ [ow, fromMaybe emptyWidget md] ++ maybeToList tw -- TODO don't use empty widgets

drawLogbookEntries :: [LogbookEntry] -> MDrawer
drawLogbookEntries [] = pure Nothing
drawLogbookEntries lbes = Just . vBox <$> mapM drawLogbookEntry lbes

drawLogbookTotal :: Maybe UTCTime -> [LogbookEntry] -> MDrawer
drawLogbookTotal Nothing [] = pure Nothing
drawLogbookTotal mopen lbes = do
  openTime <-
    forM mopen $ \open -> do
      now <- asks zonedTimeToUTC
      pure $ diffUTCTime now open
  let total = fromMaybe 0 openTime + sum (map logbookEntryDiffTime lbes)
  pure
    $ Just
    $ hBox
      [ str "TOTAL: ",
        hLimit (length ("[2018-10-11 00:30:02]--[2018-10-11 00:30:09] = " :: String))
          $ vLimit 1
          $ fill ' ',
        drawNominalDiffTime total
      ]

drawLogbookEntry :: LogbookEntry -> Drawer
drawLogbookEntry lbe@LogbookEntry {..} = do
  sw <- drawLogbookTimestamp logbookEntryStart
  ew <- drawLogbookTimestamp logbookEntryEnd
  pure $
    hBox
      [str "CLOCK: ", sw, str "--", ew, str " = ", drawNominalDiffTime $ logbookEntryDiffTime lbe]

drawLogOpen :: UTCTime -> Drawer
drawLogOpen u = do
  now <- asks zonedTimeToUTC
  sw <- drawLogbookTimestamp u
  ew <- drawLogbookTimestamp now
  pure
    $ withAttr selectedAttr
    $ hBox
      [ str "CLOCK: ",
        sw,
        str "--",
        ew,
        str " = ",
        drawNominalDiffTime $ diffUTCTime now u,
        str " RUNNING"
      ]

drawLogbookTimestamp :: UTCTime -> Drawer
drawLogbookTimestamp utct = do
  tw <- drawUTCLocal utct
  pure $ str "[" <+> tw <+> str "]"

drawTodoState :: TodoState -> Widget ResourceName
drawTodoState ts =
  withAttr (todoStateSpecificAttr ts <> todoStateAttr) . textLineWidget $ todoStateText ts

drawUTCLocal :: UTCTime -> Drawer
drawUTCLocal utct = do
  tz <- asks zonedTimeZone
  let localTime = utcToLocalTime tz utct
  pure $ str (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime)

drawTextFieldCursor :: Select -> TextFieldCursor -> Widget ResourceName
drawTextFieldCursor s =
  case s of
    MaybeSelected -> selectedTextFieldCursorWidget ResourceTextCursor
    _ -> textFieldCursorWidget

drawTextCursor :: Select -> TextCursor -> Widget ResourceName
drawTextCursor s =
  case s of
    MaybeSelected -> selectedTextCursorWidget ResourceTextCursor
    _ -> textCursorWidget

drawFilePath :: Path b File -> Widget n
drawFilePath = withAttr fileAttr . str . toFilePath

drawDirPath :: Path b Dir -> Widget n
drawDirPath = withAttr dirAttr . str . toFilePath

type DrawEnv = ZonedTime

type MDrawer = Reader DrawEnv (Maybe (Widget ResourceName))

type Drawer = Drawer' (Widget ResourceName)

type Drawer' = Reader DrawEnv
