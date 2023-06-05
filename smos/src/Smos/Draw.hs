{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Cursor.Brick.Forest
import Cursor.Brick.List.NonEmpty
import Cursor.Brick.Map
import Cursor.Brick.Map.KeyValue
import Cursor.Brick.Text
import Cursor.DirForest
import Cursor.DirForest.Brick
import Cursor.FuzzyLocalTime
import Cursor.List
import qualified Cursor.List.NonEmpty as NEC
import Cursor.Map
import Cursor.Simple.List.NonEmpty hiding (NonEmptyCursor)
import Cursor.Text
import Cursor.TextField
import Cursor.Tree hiding (drawTreeCursor)
import Data.Foldable
import Data.FuzzyTime
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Version (showVersion)
import Lens.Micro
import Path
import Paths_smos
import Smos.Actions
import Smos.Cursor.FileBrowser
import Smos.Cursor.SmosFileEditor
import Smos.Cursor.Tag
import Smos.Data
import Smos.Draw.Base
import Smos.Draw.Report
import Smos.History
import Smos.Keys
import Smos.Report.OptParse.Types
import Smos.Style
import Smos.Types
import Text.Time.Pretty

smosDraw :: Path Abs Dir -> SmosConfig -> SmosState -> [Widget ResourceName]
smosDraw workflowDir SmosConfig {..} SmosState {..} =
  let denv =
        DrawEnv
          { drawEnvWaitingThreshold = waitingReportSettingThreshold $ reportSettingWaitingSettings configReportSettings,
            drawEnvStuckThreshold = stuckReportSettingThreshold $ reportSettingStuckSettings configReportSettings,
            drawEnvWorkDrawEnv =
              let WorkReportSettings {..} = reportSettingWorkSettings configReportSettings
               in DrawWorkEnv
                    { drawWorkEnvProjection = workReportSettingProjection
                    },
            drawEnvNow = smosStateTime
          }
   in [ vBox $
          concat
            [ [ padBottom Max $ runReader (drawEditorCursor workflowDir configKeyMap smosStateCursor) denv,
                drawErrorMessages smosStateErrorMessages
              ],
              [ drawExplainerMode smosStateDebugInfo | configExplainerMode
              ]
            ]
      ]

drawEditorCursor :: Path Abs Dir -> KeyMap -> EditorCursor -> Drawer
drawEditorCursor workflowDir configKeyMap EditorCursor {..} =
  case editorCursorSelection of
    HelpSelected -> pure $ drawHelpCursor configKeyMap MaybeSelected editorCursorHelpCursor
    FileSelected -> case editorCursorFileCursor of
      Nothing -> pure $ str "No file selected" -- TODO make this a nice view
      Just sfec -> drawFileEditorCursor workflowDir configKeyMap MaybeSelected sfec
    BrowserSelected -> case editorCursorBrowserCursor of
      Nothing -> pure $ str "No directory selected" -- TODO make this a nice view
      Just fbc -> pure $ drawFileBrowserCursor workflowDir configKeyMap MaybeSelected fbc
    ReportSelected -> case editorCursorReportCursor of
      Nothing -> pure $ str "No report selected" -- TODO make this a nice view
      Just rc -> drawReportCursor MaybeSelected rc

drawErrorMessages :: [Text] -> Widget n
drawErrorMessages = vBox . map (withAttr errorAttr . txtWrap)

drawExplainerMode :: DebugInfo -> Widget n
drawExplainerMode DebugInfo {..} =
  vBox
    [ hBorderWithLabel (str "[ Explainer mode ]"),
      vLimit 2 $
        case NE.head <$> debugInfoLastMatches of
          Nothing -> str "Nothing happened yet."
          Just ActivationDebug {..} ->
            vBox
              [ hBox
                  [ str "Last activated action: ",
                    drawActionName activationDebugName,
                    str " (",
                    drawKeyPresses activationDebugMatch,
                    str ")"
                  ]
              ]
    ]

drawKeyPresses :: Seq KeyPress -> Widget n
drawKeyPresses = withAttr keyAttr . hBox . intersperse (str " ") . map (txt . renderKeyPress) . toList

drawInfo :: KeyMap -> Widget n
drawInfo km =
  withAttr selectedAttr $
    B.vCenterLayer $
      vBox $
        map B.hCenterLayer $
          concat
            [ [ str "SMOS",
                str " ",
                str "by Tom Sydney Kerckhove",
                str "Smos is open source and freely distributable.",
                str " ",
                str "Building smos takes time, energy and money.",
                str "Please consider supporting the project.",
                str "https://docs.smos.online/support"
              ],
              case lookupStartingActionInKeymap of
                Nothing -> []
                Just kpt ->
                  [ str " ",
                    str " ",
                    str "If you don't know what to do,",
                    hBox
                      [ str "activate the ",
                        drawActionName $ actionName selectHelp,
                        str " action using the ",
                        withAttr keyAttr $ txt kpt,
                        str " key."
                      ],
                    str "for an overview of the available commands",
                    str "or visit https://docs.smos.online"
                  ],
              [ str " ",
                str $ "Version " <> showVersion version
              ]
            ]
  where
    lookupStartingActionInKeymap :: Maybe Text
    lookupStartingActionInKeymap = lookupActionByName (keyMapAnyKeyMap km) (actionName selectHelp)

lookupActionByName :: KeyMappings -> ActionName -> Maybe Text
lookupActionByName matchers an =
  let matchAction a = actionName a == an
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
   in msum $ map matchKeyMapping matchers

drawHelpCursor :: KeyMap -> Select -> Maybe HelpCursor -> Widget ResourceName
drawHelpCursor km _ Nothing = drawInfo km
drawHelpCursor km s (Just HelpCursor {..}) =
  ( hBorderWithLabel
      (withAttr selectedAttr $ txt ("[Help page for context: " <> helpCursorTitle <> "]"))
      <=>
  )
    $ vBox
    $ concat
      [ [ hBox
            [ vBox
                [ padAll 1 $
                    viewport ResourceViewport Vertical $
                      case helpCursorSelectedKeyHelpCursors of
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
                            hBox [txt "Key Bindings: ", drawKeyCombinations keyHelpCursorKeyBinding],
                            txt "Description: ",
                            withAttr helpDescriptionAttr $ txtWrap keyHelpCursorDescription
                          ]
            ]
        ],
        case lookupExitHelpActionInKeymap of
          Nothing -> []
          Just kpt ->
            [ hBorder,
              hBox
                [ str "To exit the help screen, activate the ",
                  drawActionName $ actionName exitHelp,
                  str " action using the ",
                  withAttr keyAttr $ txt kpt,
                  str " key."
                ]
            ]
      ]
  where
    lookupExitHelpActionInKeymap :: Maybe Text
    lookupExitHelpActionInKeymap = lookupActionByName (helpKeyMapAnyMatchers $ keyMapHelpKeyMap km) (actionName exitHelp)
    go :: Select -> KeyHelpCursor -> [Widget n]
    go s_ KeyHelpCursor {..} =
      let msel = withVisibleSelected s_ . withSelPointer s_
       in [ drawKeyCombinations keyHelpCursorKeyBinding,
            msel $ withAttr helpNameAttr $ textWidget $ actionNameText keyHelpCursorName
          ]

drawKeyCombinations :: [KeyCombination] -> Widget n
drawKeyCombinations kbs =
  hBox $
    intersperse (str ", ") $
      map (withAttr helpKeyCombinationAttr . drawKeyCombination) kbs

drawKeyCombination :: KeyCombination -> Widget n
drawKeyCombination = txt . renderKeyCombination

defaultPadding :: Padding
defaultPadding = Pad defaultPaddingAmount

defaultPaddingAmount :: Int
defaultPaddingAmount = 2

drawFileBrowserCursor :: Path Abs Dir -> KeyMap -> Select -> FileBrowserCursor -> Widget ResourceName
drawFileBrowserCursor workflowDir keyMap s FileBrowserCursor {..} =
  withHeading (str "File Browser: " <+> drawOpenedDirPath workflowDir fileBrowserCursorBase) $
    padAll 1 $
      case fileBrowserCursorDirForestCursor of
        Nothing -> drawInfo keyMap
        Just fbc ->
          vBox
            [ viewport ResourceViewport Vertical $
                verticalPaddedDirForestCursorWidget
                  (goFodCursor s)
                  goFodUnselected
                  defaultPaddingAmount
                  fbc,
              let ms =
                    case fileBrowserCursorSelection of
                      FileBrowserFilterSelected -> s
                      FileBrowserSelected -> NotSelected
               in hBox
                    [ textLineWidget "Filter:",
                      txt " ",
                      drawTextCursor ms fileBrowserCursorFilterBar
                    ]
            ]
  where
    goFodCursor :: Select -> FileOrDirCursor () -> Widget ResourceName
    goFodCursor ms = \case
      InProgress tc ->
        ( case ms of
            MaybeSelected -> addPointer
            NotSelected -> addLister
        )
          $ drawTextCursor ms tc
      Existent fod -> case ms of
        MaybeSelected -> goFodSelected fod
        NotSelected -> goFodUnselected fod
    goFodSelected :: FileOrDir () -> Widget ResourceName
    goFodSelected = addPointer . goFod
    addPointer :: Widget n -> Widget n
    addPointer = forceAttr selectedAttr . visible . (str [pointerChar, ' '] <+>)
    goFodUnselected :: FileOrDir () -> Widget ResourceName
    goFodUnselected = addLister . goFod
    addLister :: Widget n -> Widget n
    addLister = (str [listerChar, ' '] <+>)
    goFod :: FileOrDir () -> Widget ResourceName
    goFod fod = case fod of
      FodFile rf () -> fileStyle rf $ drawFilePath rf
      FodDir rd -> drawDirPath rd
    fileStyle :: Path Rel File -> Widget n -> Widget n
    fileStyle rf =
      case fileExtension rf of
        Just ".smos" -> forceAttr fileAttr -- TODO maybe also something fancy?
        _ -> forceAttr nonSmosFileAttr

drawFileEditorCursor :: Path Abs Dir -> KeyMap -> Select -> SmosFileEditorCursor -> Drawer
drawFileEditorCursor workflowDir keyMap s SmosFileEditorCursor {..} = do
  w <- case historyPresent smosFileEditorCursorHistory of
    Nothing -> pure $ drawInfo keyMap
    Just sfc -> drawSmosFileCursor s sfc
  let heading =
        hBox
          [ str "Editor: ",
            drawOpenedFilePath workflowDir smosFileEditorPath,
            if smosFileEditorUnsavedChanges
              then withAttr unsavedAttr (str " [+]")
              else emptyWidget
          ]
  pure $ withHeading heading w

drawOpenedFilePath :: Path Abs Dir -> Path Abs File -> Widget n
drawOpenedFilePath workflowDir currentFile = case stripProperPrefix workflowDir currentFile of
  Nothing -> drawFilePath currentFile
  Just rf -> drawFilePath $ workflowRelDir </> rf

drawOpenedDirPath :: Path Abs Dir -> Path Abs Dir -> Widget n
drawOpenedDirPath workflowDir currentDir =
  if workflowDir == currentDir
    then drawDirPath workflowRelDir
    else case stripProperPrefix workflowDir currentDir of
      Nothing -> drawDirPath currentDir
      Just rd -> drawDirPath $ workflowRelDir </> rd

workflowRelDir :: Path Rel Dir
workflowRelDir = [reldir|workflow|]

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
           in (,)
                <$> drawEntryCursor s TreeIsNotCollapsed edc ec
                <*> pure (rebuildEntryCursor $ collapseEntryValue ec, edc)
        ClosedForest ts ->
          let edc = makeClosedEntryDrawContext ts
           in (,)
                <$> drawEntryCursor s TreeIsCollapsed edc ec
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

newtype EntryDrawContext = EntryDrawContext
  { entryDrawContextForest :: Forest Entry
  }
  deriving (Show, Eq)

entryDrawContextDirectChildren :: EntryDrawContext -> Word
entryDrawContextDirectChildren = genericLength . entryDrawContextForest

entryDrawContextLegacy :: EntryDrawContext -> Word
entryDrawContextLegacy = forestSize . entryDrawContextForest
  where
    forestSize :: Forest a -> Word
    forestSize = foldl' (+) 0 . map treeSize
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

completedForestNumbersWidget :: EntryDrawContext -> Maybe (Widget t)
completedForestNumbersWidget edc =
  let es@EntryStats {..} = goF (entryDrawContextForest edc)
   in if es == mempty
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

data EntryStats = EntryStats
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

collapsedForestNumbersWidget :: TreeCollapsing -> EntryDrawContext -> Maybe (Widget t)
collapsedForestNumbersWidget tc edc =
  case tc of
    TreeIsNotCollapsed -> Nothing
    TreeIsCollapsed ->
      let direct = entryDrawContextDirectChildren edc
          legacy = entryDrawContextLegacy edc
       in Just $
            str $
              unwords
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
  tscw <- drawIfM collapseEntryShowTimestamps e <$> forM entryCursorTimestampsCursor (drawTimestampsCursor (selectWhen TimestampsSelected))
  lbcw <- drawIfM collapseEntryShowLogbook e <$> drawLogbookCursor (selectWhen LogbookSelected) entryCursorLogbookCursor
  shcw <-
    drawIfM collapseEntryShowHistory e
      <$> fmap
        join
        ( forM
            entryCursorStateHistoryCursor
            ( drawStateHistoryCursor (selectWhen StateHistorySelected)
            )
        )
  let headerLine =
        hBox $
          intersperse (str " ") $
            concat
              [ [ case s of
                    NotSelected -> str [listerChar]
                    MaybeSelected -> withAttr selectedAttr $ str [pointerChar]
                ],
                maybeToList (entryCursorStateHistoryCursor >>= drawCurrentStateFromCursor),
                [drawHeaderCursor (selectWhen HeaderSelected) entryCursorHeaderCursor],
                maybeToList $ drawTagsCursor (selectWhen TagsSelected) <$> entryCursorTagsCursor,
                [ str "..."
                  | let Entry {..} = rebuildEntryCursor ec
                     in or
                          [ not (collapseEntryShowContents e) && isJust entryContents,
                            not (collapseEntryShowLogbook e) && not (nullLogbook entryLogbook),
                            not (collapseEntryShowProperties e) && not (M.null entryProperties),
                            not (collapseEntryShowTimestamps e) && not (M.null entryTimestamps)
                          ]
                ],
                maybeToList $ completedForestNumbersWidget edc,
                maybeToList $ collapsedForestNumbersWidget tc edc
              ]
  pure $
    withVisible s $
      (headerLine <=>) $
        padLeft defaultPadding $
          vBox $
            catMaybes
              [ tscw,
                drawIfM collapseEntryShowProperties e $
                  drawPropertiesCursor (selectWhen PropertiesSelected) <$> entryCursorPropertiesCursor,
                drawIfM collapseEntryShowContents e $
                  drawContentsCursor (selectWhen ContentsSelected) <$> entryCursorContentsCursor,
                lbcw,
                shcw
              ]
  where
    ec@EntryCursor {..} = collapseEntryValue e
    selectWhen :: EntryCursorSelection -> Select
    selectWhen ecs =
      s
        <> ( if ecs == entryCursorSelected
               then MaybeSelected
               else NotSelected
           )

drawEntry :: TreeCollapsing -> EntryDrawContext -> CollapseEntry Entry -> Drawer
drawEntry tc edc e = do
  tsw <- drawIfM collapseEntryShowTimestamps e <$> drawTimestamps entryTimestamps
  lbw <- drawIfM collapseEntryShowLogbook e <$> drawLogbook entryLogbook
  shw <- drawIfM collapseEntryShowHistory e <$> drawStateHistory entryStateHistory
  let headerLine =
        hBox $
          intersperse (str " ") $
            concat
              [ [str [listerChar]],
                maybeToList (drawCurrentState entryStateHistory),
                [drawHeader entryHeader],
                maybeToList (drawTags entryTags),
                [ str "..."
                  | or
                      [ not (collapseEntryShowContents e) && isJust entryContents,
                        not (collapseEntryShowLogbook e) && not (nullLogbook entryLogbook),
                        not (collapseEntryShowProperties e) && not (M.null entryProperties),
                        not (collapseEntryShowTimestamps e) && not (M.null entryTimestamps)
                      ]
                ],
                maybeToList $ completedForestNumbersWidget edc,
                maybeToList $ collapsedForestNumbersWidget tc edc
              ]
  pure $
    (headerLine <=>) $
      padLeft defaultPadding $
        vBox $
          catMaybes
            [ tsw,
              drawIfM collapseEntryShowProperties e $ drawProperties entryProperties,
              drawIfM collapseEntryShowContents e $ drawContents <$> entryContents,
              lbw,
              shw
            ]
  where
    Entry {..} = collapseEntryValue e

drawIfM :: (CollapseEntry e -> Bool) -> CollapseEntry e -> Maybe a -> Maybe a
drawIfM bf e mw = mw >>= drawIf bf e

drawIf :: (CollapseEntry e -> Bool) -> CollapseEntry e -> a -> Maybe a
drawIf bf e w =
  if bf e
    then Just w
    else Nothing

drawHeaderCursor :: Select -> HeaderCursor -> Widget ResourceName
drawHeaderCursor s = withAttr headerAttr . drawTextCursor s . headerCursorTextCursor

drawCurrentStateFromCursor :: StateHistoryCursor -> Maybe (Widget ResourceName)
drawCurrentStateFromCursor = drawCurrentState . rebuildStateHistoryCursor . Just

drawCurrentState :: StateHistory -> Maybe (Widget ResourceName)
drawCurrentState stateHistory =
  stateHistoryState stateHistory <&> \ts -> withAttr todoStateAttr $ drawTodoState ts

drawContentsCursor :: Select -> ContentsCursor -> Widget ResourceName
drawContentsCursor s = drawTextFieldCursor s . cleanTextFieldCursor . contentsCursorTextFieldCursor

cleanTextFieldCursor :: TextFieldCursor -> TextFieldCursor
cleanTextFieldCursor = TextFieldCursor . cleanNonEmptyCursorText . textFieldCursorNonEmpty
  where
    cleanNonEmptyCursorText :: NEC.NonEmptyCursor TextCursor Text -> NEC.NonEmptyCursor TextCursor Text
    cleanNonEmptyCursorText = NEC.mapNonEmptyCursor cleanTextCursor cleanText

cleanTextCursor :: TextCursor -> TextCursor
cleanTextCursor = TextCursor . cleanListCursorChar . textCursorList
  where
    cleanListCursorChar :: ListCursor Char -> ListCursor Char
    cleanListCursorChar = fmap cleanChar

drawContents :: Contents -> Widget ResourceName
drawContents = textWidget . cleanText . contentsText

-- Keep in sync with cleanChar below above
cleanText :: Text -> Text
cleanText = T.replace "\t" " " . T.replace "\r" " "

-- Keep in sync with cleanText above
cleanChar :: Char -> Char
cleanChar = \case
  '\t' -> ' '
  '\r' -> ' '
  c -> c

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
      dw <- drawTimestampWithPrettyRelative ts
      pure $
        hBox
          [ case s of
              NotSelected -> drawTimestampName $ rebuildTimestampNameCursor tc
              MaybeSelected -> maybe id (\tsn -> withAttr (timestampNameSpecificAttr tsn)) (timestampName (rebuildTextCursor tc)) $ drawTextCursor s tc,
            str ": ",
            dw
          ]
    goVal tsn fdc = do
      tsw <-
        case s of
          NotSelected -> drawTimestampWithPrettyRelative $ rebuildTimestampCursor fdc
          MaybeSelected -> drawFuzzyLocalTimeCursor s fdc
      pure $ hBox [drawTimestampName tsn, str ": ", tsw]

drawFuzzyLocalTimeCursor :: Select -> FuzzyLocalTimeCursor -> Drawer
drawFuzzyLocalTimeCursor s fdc@FuzzyLocalTimeCursor {..} = do
  dw <-
    case rebuildFuzzyLocalTimeCursor fdc of
      OnlyDaySpecified d -> drawDayWithPrettyRelative d
      BothTimeAndDay lt -> drawLocalTimeWithPrettyRelative lt
  pure
    $ ( case s of
          NotSelected -> id
          MaybeSelected -> withAttr selectedAttr
      )
    $ hBox
    $ intersperse (str " ")
    $ drawTextCursor s fuzzyLocalTimeCursorTextCursor
      : [hBox [str "(", dw, str ")"] | MaybeSelected <- [s]]

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
      let pn = fromMaybe "" $ propertyName $ rebuildTextCursor tc
       in withAttr (propertyNameSpecificAttr pn) $
            hBox [sel $ drawTextCursor s tc, str ": ", drawPropertyValue pn pv]
    goVal pn tc =
      withAttr (propertyNameSpecificAttr pn) $
        hBox [drawPropertyName pn, str ": ", sel $ drawTextCursor s tc]

drawProperties :: Map PropertyName PropertyValue -> Maybe (Widget ResourceName)
drawProperties m
  | M.null m = Nothing
  | otherwise = Just $ vBox $ map (uncurry drawPropertyPair) $ M.toList m

drawStateHistoryCursor :: Select -> StateHistoryCursor -> MDrawer
drawStateHistoryCursor _ = drawStateHistory . rebuildStateHistoryCursor . Just

drawStateHistory :: StateHistory -> MDrawer
drawStateHistory (StateHistory ls)
  | null ls = pure Nothing
  | otherwise = do
      zt <- asks drawEnvNow
      pure $
        Just $
          withAttr todoStateHistoryAttr $
            drawTable $
              flip map ls $
                \StateHistoryEntry {..} ->
                  [ maybe (str " ") drawTodoState stateHistoryEntryNewState,
                    strWrap $
                      unwords
                        [ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" stateHistoryEntryTimestamp,
                          "(" ++ prettyTimeAuto (zonedTimeToUTC zt) stateHistoryEntryTimestamp ++ ")"
                        ]
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
      now <- asks $ zonedTimeToUTC . drawEnvNow
      pure $ diffUTCTime now open
  let total = fromMaybe 0 openTime + foldl' (+) 0 (map logbookEntryDiffTime lbes)
  pure $
    Just $
      hBox
        [ str "TOTAL: ",
          hLimit (length ("[2018-10-11 00:30:02]--[2018-10-11 00:30:09] = " :: String)) $
            vLimit 1 $
              fill ' ',
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
  now <- asks $ zonedTimeToUTC . drawEnvNow
  sw <- drawLogbookTimestamp u
  ew <- drawLogbookTimestamp now
  pure $
    withAttr selectedAttr $
      hBox
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

drawUTCLocal :: UTCTime -> Drawer
drawUTCLocal utct = do
  tz <- asks $ zonedTimeZone . drawEnvNow
  let localTime = utcToLocalTime tz utct
  pure $ str (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime)

drawActionName :: ActionName -> Widget n
drawActionName = withAttr keyAttr . txt . actionNameText
