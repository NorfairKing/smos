{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Types
    ( module Smos.Types
    , module Smos.Monad
    ) where

import Import

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Time

import Lens.Micro

import Control.Monad.Reader
import Control.Monad.State

import Graphics.Vty.Input.Events as Vty

import Brick.Types as B hiding (Next)

import Cursor.Simple.List.NonEmpty

import Smos.Data

import Smos.Cursor.SmosFile

import Smos.Monad

data SmosConfig = SmosConfig
    { configKeyMap :: KeyMap
    } deriving (Generic)

data KeyMap = KeyMap
    { keyMapHelpMatchers :: KeyMappings
    , keyMapEmptyMatchers :: KeyMappings
    , keyMapEntryMatchers :: KeyMappings
    , keyMapHeaderMatchers :: KeyMappings
    , keyMapContentsMatchers :: KeyMappings
    , keyMapTimestampsMatchers :: KeyMappings
    , keyMapPropertiesMatchers :: KeyMappings
    , keyMapStateHistoryMatchers :: KeyMappings
    , keyMapTagsMatchers :: KeyMappings
    , keyMapLogbookMatchers :: KeyMappings
    , keyMapAnyMatchers :: KeyMappings
    } deriving (Generic)

instance Semigroup KeyMap where
    (<>) km1 km2 =
        KeyMap
            { keyMapHelpMatchers =
                  keyMapHelpMatchers km1 <> keyMapHelpMatchers km2
            , keyMapEmptyMatchers =
                  keyMapEmptyMatchers km1 <> keyMapEmptyMatchers km2
            , keyMapEntryMatchers =
                  keyMapEntryMatchers km1 <> keyMapEntryMatchers km2
            , keyMapHeaderMatchers =
                  keyMapHeaderMatchers km1 <> keyMapHeaderMatchers km2
            , keyMapContentsMatchers =
                  keyMapContentsMatchers km1 <> keyMapContentsMatchers km2
            , keyMapTimestampsMatchers =
                  keyMapTimestampsMatchers km1 <> keyMapTimestampsMatchers km2
            , keyMapPropertiesMatchers =
                  keyMapPropertiesMatchers km1 <> keyMapPropertiesMatchers km2
            , keyMapStateHistoryMatchers =
                  keyMapStateHistoryMatchers km1 <>
                  keyMapStateHistoryMatchers km2
            , keyMapTagsMatchers =
                  keyMapTagsMatchers km1 <> keyMapTagsMatchers km2
            , keyMapLogbookMatchers =
                  keyMapLogbookMatchers km1 <> keyMapLogbookMatchers km2
            , keyMapAnyMatchers = keyMapAnyMatchers km1 <> keyMapAnyMatchers km2
            }

instance Monoid KeyMap where
    mempty =
        KeyMap
            { keyMapHelpMatchers = mempty
            , keyMapEmptyMatchers = mempty
            , keyMapEntryMatchers = mempty
            , keyMapHeaderMatchers = mempty
            , keyMapContentsMatchers = mempty
            , keyMapTimestampsMatchers = mempty
            , keyMapPropertiesMatchers = mempty
            , keyMapStateHistoryMatchers = mempty
            , keyMapTagsMatchers = mempty
            , keyMapLogbookMatchers = mempty
            , keyMapAnyMatchers = mempty
            }

type KeyMappings = [KeyMapping]

data KeyMapping
    = MapVtyExactly KeyPress
                    Action
    | MapAnyTypeableChar (ActionUsing Char)
    | MapCatchAll Action
    | MapCombination KeyPress
                     KeyMapping

data Action = Action
    { actionName :: Text
    , actionFunc :: SmosM ()
    , actionDescription :: Text
    } deriving (Generic)

action :: Text -> SmosM () -> Action
action name func =
    Action {actionName = name, actionFunc = func, actionDescription = ""}

data ActionUsing a = ActionUsing
    { actionUsingName :: Text
    , actionUsingFunc :: a -> SmosM ()
    , actionUsingDescription :: Text
    } deriving (Generic)

instance Contravariant ActionUsing where
    contramap func a = a {actionUsingFunc = \b -> actionUsingFunc a $ func b}

actionUsing :: Text -> (a -> SmosM ()) -> ActionUsing a
actionUsing name func =
    ActionUsing
        { actionUsingName = name
        , actionUsingFunc = func
        , actionUsingDescription = ""
        }

data AnyAction
    = PlainAction Action
    | UsingCharAction (ActionUsing Char)

type SmosEvent = BrickEvent ResourceName ()

type SmosM = MkSmosM SmosConfig ResourceName SmosState

runSmosM ::
       SmosConfig
    -> SmosState
    -> SmosM a
    -> EventM ResourceName (MStop a, SmosState)
runSmosM = runMkSmosM

data SmosState = SmosState
    { smosStateStartSmosFile :: Maybe SmosFile
    , smosStateTimeZone :: TimeZone
    , smosStateFilePath :: Path Abs File
    , smosStateCursor :: EditorCursor
    , smosStateKeyHistory :: Seq KeyPress
    , smosStateCursorHistory :: [EditorCursor] -- From youngest to oldest
    , smosStateDebugInfo :: DebugInfo
    } deriving (Generic)

data KeyPress =
    KeyPress Key
             [Modifier]
    deriving (Show, Eq, Ord,Generic)

instance Validity KeyPress where
    validate _ = valid -- TODO no validity instances for VTY types

data DebugInfo = DebugInfo
    { debugInfoLastMatches :: Maybe (NonEmpty ActivationDebug)
    } deriving (Show, Eq, Generic)

data ActivationDebug = ActivationDebug
    { activationDebugPrecedence :: Precedence
    , activationDebugPriority :: Priority
    , activationDebugMatch :: Seq KeyPress
    , activationDebugName :: Text
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
type HelpCursor = NonEmptyCursor KeyHelpCursor

makeHelpCursor :: KeyMappings -> Maybe HelpCursor
makeHelpCursor = fmap (makeNonEmptyCursor) . NE.nonEmpty . map go
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

data KeyHelpCursor = KeyHelpCursor
    { keyHelpCursorKeyBinding :: KeyCombination
    , keyHelpCursorName :: Text
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
    , editorCursorHelpCursor :: Maybe HelpCursor
    , editorCursorSelection :: EditorSelection
    , editorCursorDebug :: Bool
    } deriving (Show, Eq, Generic)

instance Validity EditorCursor

-- [ Editor Cursor ] --
--
-- Cannot factor this out because of the problem with help cursor.
data EditorSelection
    = EditorSelected
    | HelpSelected
    deriving (Show, Eq, Generic)

instance Validity EditorSelection

makeEditorCursor :: SmosFile -> EditorCursor
makeEditorCursor sf =
    EditorCursor
        { editorCursorFileCursor =
              fmap makeSmosFileCursor $ NE.nonEmpty $ smosFileForest sf
        , editorCursorHelpCursor = Nothing
        , editorCursorSelection = EditorSelected
        , editorCursorDebug = False
        }

rebuildEditorCursor :: EditorCursor -> SmosFile
rebuildEditorCursor =
    SmosFile .
    maybe [] NE.toList . fmap rebuildSmosFileCursor . editorCursorFileCursor

editorCursorSmosFileCursorL :: Lens' EditorCursor (Maybe SmosFileCursor)
editorCursorSmosFileCursorL =
    lens editorCursorFileCursor $ \ec msfc -> ec {editorCursorFileCursor = msfc}

editorCursorSelectionL :: Lens' EditorCursor EditorSelection
editorCursorSelectionL =
    lens editorCursorSelection $ \ec es -> ec {editorCursorSelection = es}

editorCursorSelectEditor :: EditorCursor -> EditorCursor
editorCursorSelectEditor = editorCursorSelectionL .~ EditorSelected

editorCursorSelectHelp :: EditorCursor -> EditorCursor
editorCursorSelectHelp = editorCursorSelectionL .~ HelpSelected

editorCursorDebugL :: Lens' EditorCursor Bool
editorCursorDebugL =
    lens editorCursorDebug $ \ec sh -> ec {editorCursorDebug = sh}

editorCursorShowDebug :: EditorCursor -> EditorCursor
editorCursorShowDebug = editorCursorDebugL .~ True

editorCursorHideDebug :: EditorCursor -> EditorCursor
editorCursorHideDebug = editorCursorDebugL .~ False

editorCursorToggleDebug :: EditorCursor -> EditorCursor
editorCursorToggleDebug = editorCursorDebugL %~ not
