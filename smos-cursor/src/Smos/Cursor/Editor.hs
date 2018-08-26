{-# LANGUAGE DeriveGeneric #-}

module Smos.Cursor.Editor
    ( EditorCursor(..)
    , EditorSelection(..)
    , makeEditorCursor
    , rebuildEditorCursor
    , editorCursorSmosFileCursorL
    , editorCursorSelectionL
    , editorCursorSelectEditor
    , editorCursorSelectHelp
    , editorCursorDebugL
    , editorCursorShowDebug
    , editorCursorHideDebug
    , editorCursorToggleDebug
    ) where

import GHC.Generics (Generic)

import Data.Validity

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

import Lens.Micro

import Smos.Data

import Smos.Cursor.SmosFile

data EditorCursor = EditorCursor
    { editorCursorFileCursor :: Maybe SmosFileCursor
    , editorCursorSelection :: EditorSelection
    , editorCursorDebug :: Bool
    } deriving (Show, Eq, Generic)

instance Validity EditorCursor

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
