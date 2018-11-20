{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Types
    ( module Smos.Types
    , module Smos.Monad
    ) where

import Import

import Data.Aeson
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Time
import System.FileLock

import Lens.Micro

import Control.Monad.Reader
import Control.Monad.State

import Graphics.Vty.Input.Events as Vty

import Brick.Types as B hiding (Next)

import Cursor.Simple.List.NonEmpty

import Smos.Data

import Smos.Cursor.Entry
import Smos.Cursor.SmosFile

import Smos.Cursor.Report.Next
import Smos.Report.Config

import Smos.Monad

data SmosConfig = SmosConfig
    { configKeyMap :: !KeyMap
    , configReportConfig :: !SmosReportConfig
    } deriving (Generic)

data KeyMap = KeyMap
    { keyMapFileKeyMap :: !FileKeyMap
    , keyMapReportsKeyMap :: !ReportsKeyMap
    , keyMapHelpMatchers :: !KeyMappings
    } deriving (Generic)

instance Semigroup KeyMap where
    (<>) km1 km2 =
        KeyMap
        { keyMapFileKeyMap = keyMapFileKeyMap km1 <> keyMapFileKeyMap km2
        , keyMapReportsKeyMap =
              keyMapReportsKeyMap km1 <> keyMapReportsKeyMap km2
        , keyMapHelpMatchers = keyMapHelpMatchers km1 <> keyMapHelpMatchers km2
        }

instance Monoid KeyMap where
    mempty =
        KeyMap
        { keyMapFileKeyMap = mempty
        , keyMapReportsKeyMap = mempty
        , keyMapHelpMatchers = mempty
        }

data FileKeyMap = FileKeyMap
    { fileKeyMapEmptyMatchers :: !KeyMappings
    , fileKeyMapEntryMatchers :: !KeyMappings
    , fileKeyMapHeaderMatchers :: !KeyMappings
    , fileKeyMapContentsMatchers :: !KeyMappings
    , fileKeyMapTimestampsMatchers :: !KeyMappings
    , fileKeyMapPropertiesMatchers :: !KeyMappings
    , fileKeyMapStateHistoryMatchers :: !KeyMappings
    , fileKeyMapTagsMatchers :: !KeyMappings
    , fileKeyMapLogbookMatchers :: !KeyMappings
    , fileKeyMapAnyMatchers :: !KeyMappings
    } deriving (Generic)

instance Semigroup FileKeyMap where
    (<>) km1 km2 =
        FileKeyMap
        { fileKeyMapEmptyMatchers =
              fileKeyMapEmptyMatchers km1 <> fileKeyMapEmptyMatchers km2
        , fileKeyMapEntryMatchers =
              fileKeyMapEntryMatchers km1 <> fileKeyMapEntryMatchers km2
        , fileKeyMapHeaderMatchers =
              fileKeyMapHeaderMatchers km1 <> fileKeyMapHeaderMatchers km2
        , fileKeyMapContentsMatchers =
              fileKeyMapContentsMatchers km1 <> fileKeyMapContentsMatchers km2
        , fileKeyMapTimestampsMatchers =
              fileKeyMapTimestampsMatchers km1 <>
              fileKeyMapTimestampsMatchers km2
        , fileKeyMapPropertiesMatchers =
              fileKeyMapPropertiesMatchers km1 <>
              fileKeyMapPropertiesMatchers km2
        , fileKeyMapStateHistoryMatchers =
              fileKeyMapStateHistoryMatchers km1 <>
              fileKeyMapStateHistoryMatchers km2
        , fileKeyMapTagsMatchers =
              fileKeyMapTagsMatchers km1 <> fileKeyMapTagsMatchers km2
        , fileKeyMapLogbookMatchers =
              fileKeyMapLogbookMatchers km1 <> fileKeyMapLogbookMatchers km2
        , fileKeyMapAnyMatchers =
              fileKeyMapAnyMatchers km1 <> fileKeyMapAnyMatchers km2
        }

instance Monoid FileKeyMap where
    mempty =
        FileKeyMap
        { fileKeyMapEmptyMatchers = mempty
        , fileKeyMapEntryMatchers = mempty
        , fileKeyMapHeaderMatchers = mempty
        , fileKeyMapContentsMatchers = mempty
        , fileKeyMapTimestampsMatchers = mempty
        , fileKeyMapPropertiesMatchers = mempty
        , fileKeyMapStateHistoryMatchers = mempty
        , fileKeyMapTagsMatchers = mempty
        , fileKeyMapLogbookMatchers = mempty
        , fileKeyMapAnyMatchers = mempty
        }

data ReportsKeyMap = ReportsKeyMap
    { reportsKeymapNextActionReportMatchers :: !KeyMappings
    } deriving (Generic)

instance Semigroup ReportsKeyMap where
    rkm1 <> rkm2 =
        ReportsKeyMap
        { reportsKeymapNextActionReportMatchers =
              reportsKeymapNextActionReportMatchers rkm1 <>
              reportsKeymapNextActionReportMatchers rkm2
        }

instance Monoid ReportsKeyMap where
    mempty = ReportsKeyMap {reportsKeymapNextActionReportMatchers = mempty}

type KeyMappings = [KeyMapping]

data KeyMapping
    = MapVtyExactly !KeyPress
                    !Action
    | MapAnyTypeableChar !(ActionUsing Char)
    | MapCatchAll !Action
    | MapCombination !KeyPress
                     !KeyMapping

newtype ActionName = ActionName
    { actionNameText :: Text
    } deriving ( Show,Read
               , Eq,Ord
               , Generic
               , IsString
               , Semigroup
               , Monoid
               , FromJSON
               , ToJSON
               )

instance Validity ActionName

data Action = Action
    { actionName :: ActionName
    , actionFunc :: SmosM ()
    , actionDescription :: Text
    } deriving (Generic)

data ActionUsing a = ActionUsing
    { actionUsingName :: ActionName
    , actionUsingFunc :: a -> SmosM ()
    , actionUsingDescription :: Text
    } deriving (Generic)

instance Contravariant ActionUsing where
    contramap func a = a {actionUsingFunc = \b -> actionUsingFunc a $ func b}

data AnyAction
    = PlainAction Action
    | UsingCharAction (ActionUsing Char)

anyActionName :: AnyAction -> ActionName
anyActionName (PlainAction a) = actionName a
anyActionName (UsingCharAction au) = actionUsingName au

type Event = BrickEvent ResourceName SmosEvent

data SmosEvent
    = SmosUpdateTime
    | SmosSaveFile

type SmosM = MkSmosM SmosConfig ResourceName SmosState

runSmosM ::
       SmosConfig
    -> SmosState
    -> SmosM a
    -> EventM ResourceName (MStop a, SmosState)
runSmosM = runMkSmosM

data SmosState = SmosState
    { smosStateTime :: ZonedTime
    , smosStateStartSmosFile :: Maybe SmosFile
    , smosStateFilePath :: Path Abs File
    , smosStateFileLock :: FileLock
    , smosStateCursor :: EditorCursor
    , smosStateKeyHistory :: Seq KeyPress
    , smosStateCursorHistory :: [EditorCursor] -- From youngest to oldest
    , smosStateDebugInfo :: DebugInfo
    } deriving (Generic)

data KeyPress =
    KeyPress Key
             [Modifier]
    deriving (Show, Eq, Ord, Generic)

instance Validity KeyPress where
    validate _ = valid -- TODO no validity instances for VTY types

data DebugInfo = DebugInfo
    { debugInfoLastMatches :: Maybe (NonEmpty ActivationDebug)
    } deriving (Show, Eq, Generic)

data ActivationDebug = ActivationDebug
    { activationDebugPrecedence :: Precedence
    , activationDebugPriority :: Priority
    , activationDebugMatch :: Seq KeyPress
    , activationDebugName :: ActionName
    } deriving (Show, Eq, Generic)

data Priority
    = CatchAll
    | MatchAnyChar
    | MatchExact -- Has higher priority.
    deriving (Show, Eq, Ord)

data Precedence
    = AnyMatcher -- Has higher priority.
    | SpecificMatcher
    deriving (Show, Eq, Ord)

newtype ResourceName =
    ResourceName Text
    deriving (Show, Eq, Ord, Generic, IsString)

stop :: Action
stop =
    Action
    { actionName = "stop"
    , actionDescription = "Stop Smos"
    , actionFunc = MkSmosM $ NextT $ pure Stop
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
data HelpCursor = HelpCursor
    { helpCursorTitle :: Text
    , helpCursorKeyHelpCursors :: NonEmptyCursor KeyHelpCursor
    } deriving (Show, Eq, Generic)

instance Validity HelpCursor

makeHelpCursor :: Text -> KeyMappings -> Maybe HelpCursor
makeHelpCursor title kms = do
    ne <- NE.nonEmpty $ map go kms
    pure $
        HelpCursor
        { helpCursorTitle = title
        , helpCursorKeyHelpCursors = makeNonEmptyCursor ne
        }
  where
    go :: KeyMapping -> KeyHelpCursor
    go km =
        case km of
            MapVtyExactly kp a ->
                KeyHelpCursor
                { keyHelpCursorKeyBinding = PressExactly kp
                , keyHelpCursorName = actionName a
                , keyHelpCursorDescription = actionDescription a
                }
            MapAnyTypeableChar au ->
                KeyHelpCursor
                { keyHelpCursorKeyBinding = PressAnyChar
                , keyHelpCursorName = actionUsingName au
                , keyHelpCursorDescription = actionUsingDescription au
                }
            MapCatchAll a ->
                KeyHelpCursor
                { keyHelpCursorKeyBinding = PressAny
                , keyHelpCursorName = actionName a
                , keyHelpCursorDescription = actionDescription a
                }
            MapCombination kp km_ ->
                let khc = go km_
                in khc
                   { keyHelpCursorKeyBinding =
                         PressCombination kp $ keyHelpCursorKeyBinding khc
                   }

helpCursorKeyHelpCursorsL :: Lens' HelpCursor (NonEmptyCursor KeyHelpCursor)
helpCursorKeyHelpCursorsL =
    lens helpCursorKeyHelpCursors $ \hc ne -> hc {helpCursorKeyHelpCursors = ne}

helpCursorUp :: HelpCursor -> Maybe HelpCursor
helpCursorUp = helpCursorKeyHelpCursorsL nonEmptyCursorSelectPrev

helpCursorDown :: HelpCursor -> Maybe HelpCursor
helpCursorDown = helpCursorKeyHelpCursorsL nonEmptyCursorSelectNext

helpCursorStart :: HelpCursor -> HelpCursor
helpCursorStart = helpCursorKeyHelpCursorsL %~ nonEmptyCursorSelectFirst

helpCursorEnd :: HelpCursor -> HelpCursor
helpCursorEnd = helpCursorKeyHelpCursorsL %~ nonEmptyCursorSelectLast

data KeyHelpCursor = KeyHelpCursor
    { keyHelpCursorKeyBinding :: KeyCombination
    , keyHelpCursorName :: ActionName
    , keyHelpCursorDescription :: Text
    } deriving (Show, Eq, Generic)

instance Validity KeyHelpCursor

data KeyCombination
    = PressExactly KeyPress
    | PressAnyChar
    | PressAny
    | PressCombination KeyPress
                       KeyCombination
    deriving (Show, Eq, Generic)

instance Validity KeyCombination

data EditorCursor = EditorCursor
    { editorCursorFileCursor :: Maybe SmosFileCursor
    , editorCursorReportCursor :: Maybe ReportCursor
    , editorCursorHelpCursor :: Maybe HelpCursor
    , editorCursorSelection :: EditorSelection
    , editorCursorDebug :: Bool
    } deriving (Show, Eq, Generic)

instance Validity EditorCursor

-- [ Editor Cursor ] --
--
-- Cannot factor this out because of the problem with help cursor.
data EditorSelection
    = FileSelected
    | ReportSelected
    | HelpSelected
    deriving (Show, Eq, Generic)

instance Validity EditorSelection

makeEditorCursor :: SmosFile -> EditorCursor
makeEditorCursor sf =
    EditorCursor
    { editorCursorFileCursor =
          fmap makeSmosFileCursor $ NE.nonEmpty $ smosFileForest sf
    , editorCursorReportCursor = Nothing
    , editorCursorHelpCursor = Nothing
    , editorCursorSelection = FileSelected
    , editorCursorDebug = False
    }

rebuildEditorCursor :: EditorCursor -> SmosFile
rebuildEditorCursor =
    maybe emptySmosFile rebuildSmosFileCursorEntirely . editorCursorFileCursor

editorCursorSmosFileCursorL :: Lens' EditorCursor (Maybe SmosFileCursor)
editorCursorSmosFileCursorL =
    lens editorCursorFileCursor $ \ec msfc -> ec {editorCursorFileCursor = msfc}

editorCursorHelpCursorL :: Lens' EditorCursor (Maybe HelpCursor)
editorCursorHelpCursorL =
    lens editorCursorHelpCursor $ \ec msfc -> ec {editorCursorHelpCursor = msfc}

editorCursorReportCursorL :: Lens' EditorCursor (Maybe ReportCursor)
editorCursorReportCursorL =
    lens editorCursorReportCursor $ \ec msfc ->
        ec {editorCursorReportCursor = msfc}

editorCursorSelectionL :: Lens' EditorCursor EditorSelection
editorCursorSelectionL =
    lens editorCursorSelection $ \ec es -> ec {editorCursorSelection = es}

editorCursorDebugL :: Lens' EditorCursor Bool
editorCursorDebugL =
    lens editorCursorDebug $ \ec sh -> ec {editorCursorDebug = sh}

editorCursorShowDebug :: EditorCursor -> EditorCursor
editorCursorShowDebug = editorCursorDebugL .~ True

editorCursorHideDebug :: EditorCursor -> EditorCursor
editorCursorHideDebug = editorCursorDebugL .~ False

editorCursorToggleDebug :: EditorCursor -> EditorCursor
editorCursorToggleDebug = editorCursorDebugL %~ not

editorCursorSwitchToFile :: EditorCursor -> EditorCursor
editorCursorSwitchToFile ec =
    ec
    { editorCursorHelpCursor = Nothing
    , editorCursorReportCursor = Nothing
    , editorCursorSelection = FileSelected
    }

editorCursorSwitchToHelp :: KeyMap -> EditorCursor -> EditorCursor
editorCursorSwitchToHelp KeyMap {..} ec =
    ec
    { editorCursorHelpCursor =
          case editorCursorSelection ec of
              FileSelected ->
                  let FileKeyMap {..} = keyMapFileKeyMap
                  in (\(t, ms) ->
                          makeHelpCursor t $
                          ms ++ fileKeyMapAnyMatchers ++ keyMapHelpMatchers) $
                     case editorCursorFileCursor ec of
                         Nothing -> ("Empty file", fileKeyMapEmptyMatchers)
                         Just sfc ->
                             case sfc ^. smosFileCursorEntrySelectionL of
                                 WholeEntrySelected ->
                                     ("Entry", fileKeyMapEntryMatchers)
                                 HeaderSelected ->
                                     ("Header", fileKeyMapHeaderMatchers)
                                 ContentsSelected ->
                                     ("Contents", fileKeyMapContentsMatchers)
                                 TimestampsSelected ->
                                     ( "Timestamps"
                                     , fileKeyMapTimestampsMatchers)
                                 PropertiesSelected ->
                                     ( "Properties"
                                     , fileKeyMapPropertiesMatchers)
                                 StateHistorySelected ->
                                     ( "State History"
                                     , fileKeyMapStateHistoryMatchers)
                                 TagsSelected ->
                                     ("Tags", fileKeyMapTagsMatchers)
                                 LogbookSelected ->
                                     ("Logbook", fileKeyMapLogbookMatchers)
              ReportSelected ->
                  let ReportsKeyMap {..} = keyMapReportsKeyMap
                  in makeHelpCursor "Next Action Report" $
                     reportsKeymapNextActionReportMatchers
              HelpSelected -> Nothing -- Should not happen
    , editorCursorSelection = HelpSelected
    }

editorCursorSwitchToNextActionReport ::
       NextActionReportCursor -> EditorCursor -> EditorCursor
editorCursorSwitchToNextActionReport narc ec =
    ec
    { editorCursorReportCursor = Just $ ReportNextActions narc
    , editorCursorSelection = ReportSelected
    }

newtype ReportCursor =
    ReportNextActions NextActionReportCursor
    deriving (Show, Eq, Generic)

instance Validity ReportCursor
