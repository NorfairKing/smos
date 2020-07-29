{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Smos.Types
  ( module Smos.Types,
    module Smos.Monad,
  )
where

import Brick.Types as B hiding (Next)
import Control.Concurrent.Async
import Control.Monad.Reader
import Control.Monad.State
import Cursor.Simple.List.NonEmpty
import Cursor.Text
import Cursor.Types
import Data.Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time
import Import
import Lens.Micro
import Smos.Cursor.Entry
import Smos.Cursor.FileBrowser
import Smos.Cursor.Report.Next
import Smos.Cursor.SmosFile
import Smos.Cursor.SmosFileEditor
import Smos.Keys
import Smos.Monad
import Smos.Report.Config
import YamlParse.Applicative

data SmosConfig
  = SmosConfig
      { configKeyMap :: !KeyMap,
        configReportConfig :: !SmosReportConfig
      }
  deriving (Generic)

data KeyMap
  = KeyMap
      { keyMapFileKeyMap :: !FileKeyMap,
        keyMapBrowserKeyMap :: !KeyMappings,
        keyMapReportsKeyMap :: !ReportsKeyMap,
        keyMapHelpKeyMap :: !HelpKeyMap,
        keyMapAnyKeyMap :: !KeyMappings
      }
  deriving (Generic)

instance Semigroup KeyMap where
  (<>) km1 km2 =
    KeyMap
      { keyMapFileKeyMap = keyMapFileKeyMap km1 <> keyMapFileKeyMap km2,
        keyMapBrowserKeyMap = keyMapBrowserKeyMap km1 <> keyMapBrowserKeyMap km2,
        keyMapReportsKeyMap = keyMapReportsKeyMap km1 <> keyMapReportsKeyMap km2,
        keyMapHelpKeyMap = keyMapHelpKeyMap km1 <> keyMapHelpKeyMap km2,
        keyMapAnyKeyMap = keyMapAnyKeyMap km1 <> keyMapAnyKeyMap km2
      }

instance Monoid KeyMap where
  mempty =
    KeyMap {keyMapFileKeyMap = mempty, keyMapBrowserKeyMap = mempty, keyMapReportsKeyMap = mempty, keyMapHelpKeyMap = mempty, keyMapAnyKeyMap = mempty}

keyMapActions :: KeyMap -> [AnyAction]
keyMapActions (KeyMap keyMapFileKeyMap keyMapBrowserKeyMap keyMapReportsKeyMap keyMapHelpKeyMap keyMapAnyKeyMap) =
  concat
    [ fileKeyMapActions keyMapFileKeyMap,
      keyMappingsActions keyMapBrowserKeyMap,
      reportsKeyMapActions keyMapReportsKeyMap,
      helpKeyMapActions keyMapHelpKeyMap,
      keyMappingsActions keyMapAnyKeyMap
    ]

data FileKeyMap
  = FileKeyMap
      { fileKeyMapEmptyMatchers :: !KeyMappings,
        fileKeyMapEntryMatchers :: !KeyMappings,
        fileKeyMapHeaderMatchers :: !KeyMappings,
        fileKeyMapContentsMatchers :: !KeyMappings,
        fileKeyMapTimestampsMatchers :: !KeyMappings,
        fileKeyMapPropertiesMatchers :: !KeyMappings,
        fileKeyMapStateHistoryMatchers :: !KeyMappings,
        fileKeyMapTagsMatchers :: !KeyMappings,
        fileKeyMapLogbookMatchers :: !KeyMappings,
        fileKeyMapAnyMatchers :: !KeyMappings
      }
  deriving (Generic)

instance Semigroup FileKeyMap where
  (<>) km1 km2 =
    FileKeyMap
      { fileKeyMapEmptyMatchers = fileKeyMapEmptyMatchers km1 <> fileKeyMapEmptyMatchers km2,
        fileKeyMapEntryMatchers = fileKeyMapEntryMatchers km1 <> fileKeyMapEntryMatchers km2,
        fileKeyMapHeaderMatchers = fileKeyMapHeaderMatchers km1 <> fileKeyMapHeaderMatchers km2,
        fileKeyMapContentsMatchers =
          fileKeyMapContentsMatchers km1 <> fileKeyMapContentsMatchers km2,
        fileKeyMapTimestampsMatchers =
          fileKeyMapTimestampsMatchers km1 <> fileKeyMapTimestampsMatchers km2,
        fileKeyMapPropertiesMatchers =
          fileKeyMapPropertiesMatchers km1 <> fileKeyMapPropertiesMatchers km2,
        fileKeyMapStateHistoryMatchers =
          fileKeyMapStateHistoryMatchers km1 <> fileKeyMapStateHistoryMatchers km2,
        fileKeyMapTagsMatchers = fileKeyMapTagsMatchers km1 <> fileKeyMapTagsMatchers km2,
        fileKeyMapLogbookMatchers = fileKeyMapLogbookMatchers km1 <> fileKeyMapLogbookMatchers km2,
        fileKeyMapAnyMatchers = fileKeyMapAnyMatchers km1 <> fileKeyMapAnyMatchers km2
      }

instance Monoid FileKeyMap where
  mempty =
    FileKeyMap
      { fileKeyMapEmptyMatchers = mempty,
        fileKeyMapEntryMatchers = mempty,
        fileKeyMapHeaderMatchers = mempty,
        fileKeyMapContentsMatchers = mempty,
        fileKeyMapTimestampsMatchers = mempty,
        fileKeyMapPropertiesMatchers = mempty,
        fileKeyMapStateHistoryMatchers = mempty,
        fileKeyMapTagsMatchers = mempty,
        fileKeyMapLogbookMatchers = mempty,
        fileKeyMapAnyMatchers = mempty
      }

fileKeyMapActions :: FileKeyMap -> [AnyAction]
fileKeyMapActions
  ( FileKeyMap
      fileKeyMapEmptyMatchers
      fileKeyMapEntryMatchers
      fileKeyMapHeaderMatchers
      fileKeyMapContentsMatchers
      fileKeyMapTimestampsMatchers
      fileKeyMapPropertiesMatchers
      fileKeyMapStateHistoryMatchers
      fileKeyMapTagsMatchers
      fileKeyMapLogbookMatchers
      fileKeyMapAnyMatchers
    ) =
    concatMap
      keyMappingsActions
      [ fileKeyMapEmptyMatchers,
        fileKeyMapEntryMatchers,
        fileKeyMapHeaderMatchers,
        fileKeyMapContentsMatchers,
        fileKeyMapTimestampsMatchers,
        fileKeyMapPropertiesMatchers,
        fileKeyMapStateHistoryMatchers,
        fileKeyMapTagsMatchers,
        fileKeyMapLogbookMatchers,
        fileKeyMapAnyMatchers
      ]

data ReportsKeyMap
  = ReportsKeyMap
      { reportsKeymapNextActionReportMatchers :: KeyMappings,
        reportsKeymapNextActionReportFilterMatchers :: KeyMappings
      }
  deriving (Generic)

instance Semigroup ReportsKeyMap where
  rkm1 <> rkm2 =
    ReportsKeyMap
      { reportsKeymapNextActionReportMatchers =
          reportsKeymapNextActionReportMatchers rkm1 <> reportsKeymapNextActionReportMatchers rkm2,
        reportsKeymapNextActionReportFilterMatchers =
          reportsKeymapNextActionReportFilterMatchers rkm1 <> reportsKeymapNextActionReportMatchers rkm2
      }

instance Monoid ReportsKeyMap where
  mempty = ReportsKeyMap {reportsKeymapNextActionReportMatchers = mempty, reportsKeymapNextActionReportFilterMatchers = mempty}

reportsKeyMapActions :: ReportsKeyMap -> [AnyAction]
reportsKeyMapActions (ReportsKeyMap reportsKeymapNextActionReportMatchers reportsKeymapNextActionReportFilterMatchers) =
  concatMap
    keyMappingsActions
    [ reportsKeymapNextActionReportMatchers,
      reportsKeymapNextActionReportFilterMatchers
    ]

keyMapHelpMatchers :: KeyMap -> KeyMappings
keyMapHelpMatchers km =
  let HelpKeyMap {..} = keyMapHelpKeyMap km
   in helpKeyMapHelpMatchers <> helpKeyMapSearchMatchers

data HelpKeyMap
  = HelpKeyMap
      { helpKeyMapHelpMatchers :: !KeyMappings,
        helpKeyMapSearchMatchers :: !KeyMappings
      }
  deriving (Generic)

instance Semigroup HelpKeyMap where
  hkm1 <> hkm2 =
    HelpKeyMap
      { helpKeyMapHelpMatchers = helpKeyMapHelpMatchers hkm1 <> helpKeyMapHelpMatchers hkm2,
        helpKeyMapSearchMatchers = helpKeyMapSearchMatchers hkm1 <> helpKeyMapSearchMatchers hkm2
      }

instance Monoid HelpKeyMap where
  mempty = HelpKeyMap {helpKeyMapHelpMatchers = mempty, helpKeyMapSearchMatchers = mempty}

helpKeyMapActions :: HelpKeyMap -> [AnyAction]
helpKeyMapActions (HelpKeyMap helpKeyMapHelpMatchers helpKeyMapSearchMatchers) = concatMap keyMappingsActions [helpKeyMapHelpMatchers, helpKeyMapSearchMatchers]

type KeyMappings = [KeyMapping]

keyMappingsActions :: KeyMappings -> [AnyAction]
keyMappingsActions = map keyMappingAction

data KeyMapping
  = MapVtyExactly !KeyPress !Action
  | MapAnyTypeableChar !(ActionUsing Char)
  | MapCatchAll !Action
  | MapCombination !KeyPress !KeyMapping

keyMappingAction :: KeyMapping -> AnyAction
keyMappingAction = \case
  MapVtyExactly _ a -> PlainAction a
  MapAnyTypeableChar au -> UsingCharAction au
  MapCatchAll a -> PlainAction a
  MapCombination _ km -> keyMappingAction km

newtype ActionName
  = ActionName
      { actionNameText :: Text
      }
  deriving (Show, Read, Eq, Ord, Generic, IsString, Semigroup, Monoid, FromJSON, ToJSON)

instance Validity ActionName

instance YamlSchema ActionName where
  yamlSchema = ActionName <$> yamlSchema

data Action
  = Action
      { actionName :: ActionName,
        actionFunc :: SmosM (),
        actionDescription :: Text
      }
  deriving (Generic)

data ActionUsing a
  = ActionUsing
      { actionUsingName :: ActionName,
        actionUsingFunc :: a -> SmosM (),
        actionUsingDescription :: Text
      }
  deriving (Generic)

instance Contravariant ActionUsing where
  contramap func a = a {actionUsingFunc = actionUsingFunc a . func}

data AnyAction
  = PlainAction Action
  | UsingCharAction (ActionUsing Char)

anyActionName :: AnyAction -> ActionName
anyActionName (PlainAction a) = actionName a
anyActionName (UsingCharAction au) = actionUsingName au

anyActionDescription :: AnyAction -> Text
anyActionDescription (PlainAction a) = actionDescription a
anyActionDescription (UsingCharAction au) = actionUsingDescription au

type Event = BrickEvent ResourceName SmosEvent

data SmosEvent
  = SmosUpdateTime
  | SmosSaveFile

type SmosM = MkSmosM SmosConfig ResourceName SmosState

runSmosM :: SmosConfig -> SmosState -> SmosM a -> EventM ResourceName (MStop a, SmosState)
runSmosM = runMkSmosM

data SmosState
  = SmosState
      { smosStateTime :: !ZonedTime,
        smosStateCursor :: !EditorCursor,
        smosStateKeyHistory :: !(Seq KeyPress),
        smosStateAsyncs :: ![Async ()],
        smosStateDebugInfo :: !DebugInfo
      }
  deriving (Generic)

runSmosAsync :: IO () -> SmosM ()
runSmosAsync func = do
  a <- liftIO $ async func
  modify (\ss -> ss {smosStateAsyncs = a : smosStateAsyncs ss})

data DebugInfo
  = DebugInfo
      { debugInfoLastMatches :: Maybe (NonEmpty ActivationDebug)
      }
  deriving (Show, Eq, Generic)

data ActivationDebug
  = ActivationDebug
      { activationDebugPrecedence :: Precedence,
        activationDebugPriority :: Priority,
        activationDebugMatch :: Seq KeyPress,
        activationDebugName :: ActionName
      }
  deriving (Show, Eq, Generic)

data Priority
  = CatchAll
  | MatchAnyChar
  | MatchExact -- Has higher priority.
  deriving (Show, Eq, Ord)

data Precedence
  = AnyMatcher -- Has higher priority.
  | SpecificMatcher
  deriving (Show, Eq, Ord)

data ResourceName
  = ResourceTextCursor
  | ResourceViewport
  deriving (Show, Eq, Ord, Generic)

stop :: Action
stop =
  Action
    { actionName = "stop",
      actionDescription = "Stop Smos",
      actionFunc = MkSmosM $ NextT $ pure Stop
    }

-- [ Help Cursor ] --
-- I cannot factor this out because of the following circular dependency:
--
-- HelpCursor -> KeyMapping
--    ^             |
--    |             v
-- SmosState <- SmosM
--
-- and EditorCursor depends on HelpCursor, so that has the same problem
data HelpCursor
  = HelpCursor
      { helpCursorTitle :: Text,
        helpCursorSearchBar :: TextCursor,
        helpCursorSelectedKeyHelpCursors :: Maybe (NonEmptyCursor KeyHelpCursor),
        helpCursorKeyHelpCursors :: [KeyHelpCursor],
        helpCursorSelection :: HelpCursorSelection
      }
  deriving (Show, Eq, Generic)

instance Validity HelpCursor

data HelpCursorSelection
  = HelpCursorHelpSelected
  | HelpCursorSearchSelected
  deriving (Show, Eq, Generic)

instance Validity HelpCursorSelection

makeHelpCursor :: Text -> KeyMappings -> HelpCursor
makeHelpCursor title kms =
  HelpCursor
    { helpCursorTitle = title,
      helpCursorSearchBar = emptyTextCursor,
      helpCursorSelectedKeyHelpCursors = hcs,
      helpCursorKeyHelpCursors = khcs,
      helpCursorSelection = HelpCursorHelpSelected
    }
  where
    hcs = makeNonEmptyCursor <$> NE.nonEmpty khcs
    khcs = makeKeyHelpCursors kms

makeKeyHelpCursors :: KeyMappings -> [KeyHelpCursor]
makeKeyHelpCursors = combine . map makeKeyHelpCursor
  where
    combine =
      map (combineKeyHelpCursors . NE.fromList) -- Safe because of 'groupBy'
        . groupBy ((==) `on` keyHelpCursorName)
        . sortBy (compare `on` keyHelpCursorName)

makeKeyHelpCursor :: KeyMapping -> KeyHelpCursor
makeKeyHelpCursor km =
  case km of
    MapVtyExactly kp a ->
      KeyHelpCursor
        { keyHelpCursorKeyBinding = [PressExactly kp],
          keyHelpCursorName = actionName a,
          keyHelpCursorDescription = actionDescription a
        }
    MapAnyTypeableChar au ->
      KeyHelpCursor
        { keyHelpCursorKeyBinding = [PressAnyChar],
          keyHelpCursorName = actionUsingName au,
          keyHelpCursorDescription = actionUsingDescription au
        }
    MapCatchAll a ->
      KeyHelpCursor
        { keyHelpCursorKeyBinding = [PressAny],
          keyHelpCursorName = actionName a,
          keyHelpCursorDescription = actionDescription a
        }
    MapCombination kp km_ ->
      let khc = makeKeyHelpCursor km_
       in khc
            { keyHelpCursorKeyBinding = map (PressCombination kp) (keyHelpCursorKeyBinding khc)
            }

helpCursorKeySearchBarL :: Lens' HelpCursor TextCursor
helpCursorKeySearchBarL =
  lens helpCursorSearchBar $ \hc tc ->
    let query = rebuildTextCursor tc
        selected = searchHelpCursor query $ helpCursorKeyHelpCursors hc
     in hc
          { helpCursorSearchBar = tc,
            helpCursorSelectedKeyHelpCursors = makeNonEmptyCursor <$> NE.nonEmpty selected
          }

searchHelpCursor :: Text -> [KeyHelpCursor] -> [KeyHelpCursor]
searchHelpCursor query =
  filter ((T.toCaseFold query `T.isInfixOf`) . T.toCaseFold . actionNameText . keyHelpCursorName)

helpCursorSelectedKeyHelpCursorsL :: Lens' HelpCursor (Maybe (NonEmptyCursor KeyHelpCursor))
helpCursorSelectedKeyHelpCursorsL =
  lens helpCursorSelectedKeyHelpCursors $ \hc ne -> hc {helpCursorSelectedKeyHelpCursors = ne}

helpCursorUp :: HelpCursor -> Maybe HelpCursor
helpCursorUp =
  helpCursorSelectedKeyHelpCursorsL $ \msc ->
    Just
      <$> case msc of
        Nothing -> Nothing
        Just sc -> nonEmptyCursorSelectPrev sc

helpCursorDown :: HelpCursor -> Maybe HelpCursor
helpCursorDown =
  helpCursorSelectedKeyHelpCursorsL $ \msc ->
    Just
      <$> case msc of
        Nothing -> Nothing
        Just sc -> nonEmptyCursorSelectNext sc

helpCursorStart :: HelpCursor -> HelpCursor
helpCursorStart = helpCursorSelectedKeyHelpCursorsL %~ fmap nonEmptyCursorSelectFirst

helpCursorEnd :: HelpCursor -> HelpCursor
helpCursorEnd = helpCursorSelectedKeyHelpCursorsL %~ fmap nonEmptyCursorSelectLast

helpCursorSelectionL :: Lens' HelpCursor HelpCursorSelection
helpCursorSelectionL = lens helpCursorSelection $ \hc hcs -> hc {helpCursorSelection = hcs}

helpCursorSelectHelp :: HelpCursor -> Maybe HelpCursor
helpCursorSelectHelp =
  helpCursorSelectionL $ \case
    HelpCursorSearchSelected -> Just HelpCursorHelpSelected
    HelpCursorHelpSelected -> Nothing

helpCursorSelectSearch :: HelpCursor -> Maybe HelpCursor
helpCursorSelectSearch =
  helpCursorSelectionL $ \case
    HelpCursorHelpSelected -> Just HelpCursorSearchSelected
    HelpCursorSearchSelected -> Nothing

helpCursorToggleSelection :: HelpCursor -> HelpCursor
helpCursorToggleSelection =
  helpCursorSelectionL
    %~ ( \case
           HelpCursorHelpSelected -> HelpCursorSearchSelected
           HelpCursorSearchSelected -> HelpCursorHelpSelected
       )

helpCursorInsert :: Char -> HelpCursor -> Maybe HelpCursor
helpCursorInsert c = helpCursorKeySearchBarL $ textCursorInsert c

helpCursorAppend :: Char -> HelpCursor -> Maybe HelpCursor
helpCursorAppend c = helpCursorKeySearchBarL $ textCursorAppend c

helpCursorRemove :: HelpCursor -> Maybe HelpCursor
helpCursorRemove =
  helpCursorKeySearchBarL $ \tc ->
    case textCursorRemove tc of
      Nothing -> Nothing
      Just Deleted -> Nothing
      Just (Updated hc) -> Just hc

helpCursorDelete :: HelpCursor -> Maybe HelpCursor
helpCursorDelete =
  helpCursorKeySearchBarL $ \tc ->
    case textCursorDelete tc of
      Nothing -> Nothing
      Just Deleted -> Nothing
      Just (Updated hc) -> Just hc

data KeyHelpCursor
  = KeyHelpCursor
      { keyHelpCursorKeyBinding :: [KeyCombination],
        keyHelpCursorName :: ActionName,
        keyHelpCursorDescription :: Text
      }
  deriving (Show, Eq, Generic)

instance Validity KeyHelpCursor

combineKeyHelpCursors :: NonEmpty KeyHelpCursor -> KeyHelpCursor
combineKeyHelpCursors (khc :| rest) =
  khc
    { keyHelpCursorKeyBinding =
        concat (keyHelpCursorKeyBinding khc : map keyHelpCursorKeyBinding rest)
    }

data KeyCombination
  = PressExactly KeyPress
  | PressAnyChar
  | PressAny
  | PressCombination KeyPress KeyCombination
  deriving (Show, Eq, Generic)

instance Validity KeyCombination

renderKeyCombination :: KeyCombination -> Text
renderKeyCombination = go
  where
    go :: KeyCombination -> Text
    go (PressExactly kp) = renderKeyPress kp
    go PressAnyChar = "<any char>"
    go PressAny = "<any key>"
    go (PressCombination kp km) = renderKeyPress kp <> go km

-- [ Editor Cursor ] --
--
-- Cannot factor this out because of the problem with help cursor.
data EditorCursor
  = EditorCursor
      { editorCursorFileCursor :: Maybe SmosFileEditorCursor,
        editorCursorBrowserCursor :: Maybe FileBrowserCursor,
        editorCursorReportCursor :: Maybe ReportCursor,
        editorCursorHelpCursor :: Maybe HelpCursor,
        editorCursorSelection :: EditorSelection,
        editorCursorDebug :: Bool
      }

startEditorCursor :: MonadIO m => Path Abs File -> m (Maybe (Either String EditorCursor))
startEditorCursor p = do
  mErrOrCursor <- startSmosFileEditorCursor p
  let go sfec =
        EditorCursor
          { editorCursorFileCursor = Just sfec,
            editorCursorBrowserCursor = Nothing,
            editorCursorReportCursor = Nothing,
            editorCursorHelpCursor = Nothing,
            editorCursorSelection = FileSelected,
            editorCursorDebug = False
          }
  pure $ fmap (fmap go) mErrOrCursor

editorCursorFileCursorL :: Lens' EditorCursor (Maybe SmosFileEditorCursor)
editorCursorFileCursorL =
  lens editorCursorFileCursor $ \ec msfc -> ec {editorCursorFileCursor = msfc}

editorCursorBrowserCursorL :: Lens' EditorCursor (Maybe FileBrowserCursor)
editorCursorBrowserCursorL =
  lens editorCursorBrowserCursor $ \ec msfc -> ec {editorCursorBrowserCursor = msfc}

editorCursorReportCursorL :: Lens' EditorCursor (Maybe ReportCursor)
editorCursorReportCursorL =
  lens editorCursorReportCursor $ \ec msfc -> ec {editorCursorReportCursor = msfc}

editorCursorHelpCursorL :: Lens' EditorCursor (Maybe HelpCursor)
editorCursorHelpCursorL =
  lens editorCursorHelpCursor $ \ec msfc -> ec {editorCursorHelpCursor = msfc}

editorCursorSelectionL :: Lens' EditorCursor EditorSelection
editorCursorSelectionL = lens editorCursorSelection $ \ec es -> ec {editorCursorSelection = es}

editorCursorSelect :: EditorSelection -> EditorCursor -> EditorCursor
editorCursorSelect s = editorCursorSelectionL .~ s

editorCursorDebugL :: Lens' EditorCursor Bool
editorCursorDebugL = lens editorCursorDebug $ \ec sh -> ec {editorCursorDebug = sh}

editorCursorShowDebug :: EditorCursor -> EditorCursor
editorCursorShowDebug = editorCursorDebugL .~ True

editorCursorHideDebug :: EditorCursor -> EditorCursor
editorCursorHideDebug = editorCursorDebugL .~ False

editorCursorToggleDebug :: EditorCursor -> EditorCursor
editorCursorToggleDebug = editorCursorDebugL %~ not

editorCursorSwitchToHelp :: KeyMap -> EditorCursor -> EditorCursor
editorCursorSwitchToHelp km@KeyMap {..} ec =
  let withHelpBindings n ms = Just $ makeHelpCursor n $ ms ++ keyMapHelpMatchers km ++ keyMapAnyKeyMap
   in ec
        { editorCursorHelpCursor = case editorCursorSelection ec of
            FileSelected -> case editorCursorFileCursor ec of
              Nothing -> Nothing
              Just sfec ->
                let FileKeyMap {..} = keyMapFileKeyMap
                 in ( \(t, ms) ->
                        withHelpBindings t $ ms ++ fileKeyMapAnyMatchers
                    )
                      $ case smosFileEditorCursorPresent sfec of
                        Nothing -> ("Empty file", fileKeyMapEmptyMatchers)
                        Just sfc ->
                          case sfc ^. smosFileCursorEntrySelectionL of
                            WholeEntrySelected -> ("Entry", fileKeyMapEntryMatchers)
                            HeaderSelected -> ("Header", fileKeyMapHeaderMatchers)
                            ContentsSelected -> ("Contents", fileKeyMapContentsMatchers)
                            TimestampsSelected -> ("Timestamps", fileKeyMapTimestampsMatchers)
                            PropertiesSelected -> ("Properties", fileKeyMapPropertiesMatchers)
                            StateHistorySelected -> ("State History", fileKeyMapStateHistoryMatchers)
                            TagsSelected -> ("Tags", fileKeyMapTagsMatchers)
                            LogbookSelected -> ("Logbook", fileKeyMapLogbookMatchers)
            BrowserSelected -> case editorCursorBrowserCursor ec of
              Nothing -> Nothing
              Just _ -> withHelpBindings "keyMapBrowserKeyMap" keyMapBrowserKeyMap
            ReportSelected -> case editorCursorReportCursor ec of
              Nothing -> Nothing
              Just rc -> case rc of
                ReportNextActions _ ->
                  let ReportsKeyMap {..} = keyMapReportsKeyMap
                   in withHelpBindings "Next Action Report" reportsKeymapNextActionReportMatchers
            HelpSelected -> Nothing, -- Should not happen
          editorCursorSelection = HelpSelected
        }

editorCursorUpdateTime :: ZonedTime -> EditorCursor -> EditorCursor
editorCursorUpdateTime zt ec =
  ec
    { editorCursorFileCursor = smosFileEditorCursorUpdateTime zt <$> editorCursorFileCursor ec
    }

data EditorSelection
  = FileSelected
  | BrowserSelected
  | ReportSelected
  | HelpSelected
  deriving (Show, Eq, Generic)

instance Validity EditorSelection

newtype ReportCursor
  = ReportNextActions NextActionReportCursor
  deriving (Show, Eq, Generic)

instance Validity ReportCursor
