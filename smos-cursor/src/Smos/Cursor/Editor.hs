{-# LANGUAGE DeriveGeneric #-}

module Smos.Cursor.Editor
    ( EditorCursor(..)
    , makeEditorCursor
    , rebuildEditorCursor
    , editorCursorSmosFileCursorL
    , editorCursorHelpL
    , editorCursorShowHelp
    , editorCursorHideHelp
    , editorCursorToggleHelp
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
    , editorCursorHelp :: Bool
    , editorCursorDebug :: Bool
    } deriving (Show, Eq, Generic)

makeEditorCursor :: SmosFile -> EditorCursor
makeEditorCursor sf =
    EditorCursor
        { editorCursorFileCursor =
              fmap makeSmosFileCursor $ NE.nonEmpty $ smosFileForest sf
        , editorCursorHelp = False
        , editorCursorDebug = False
        }

rebuildEditorCursor :: EditorCursor -> SmosFile
rebuildEditorCursor =
    SmosFile .
    maybe [] NE.toList . fmap rebuildSmosFileCursor . editorCursorFileCursor

editorCursorSmosFileCursorL :: Lens' EditorCursor (Maybe SmosFileCursor)
editorCursorSmosFileCursorL =
    lens editorCursorFileCursor $ \ec msfc -> ec {editorCursorFileCursor = msfc}

editorCursorHelpL :: Lens' EditorCursor Bool
editorCursorHelpL = lens editorCursorHelp $ \ec sh -> ec {editorCursorHelp = sh}

editorCursorShowHelp :: EditorCursor -> EditorCursor
editorCursorShowHelp = editorCursorHelpL .~ True

editorCursorHideHelp :: EditorCursor -> EditorCursor
editorCursorHideHelp = editorCursorHelpL .~ False

editorCursorToggleHelp :: EditorCursor -> EditorCursor
editorCursorToggleHelp = editorCursorHelpL %~ not

editorCursorDebugL :: Lens' EditorCursor Bool
editorCursorDebugL =
    lens editorCursorDebug $ \ec sh -> ec {editorCursorDebug = sh}

editorCursorShowDebug :: EditorCursor -> EditorCursor
editorCursorShowDebug = editorCursorDebugL .~ True

editorCursorHideDebug :: EditorCursor -> EditorCursor
editorCursorHideDebug = editorCursorDebugL .~ False

editorCursorToggleDebug :: EditorCursor -> EditorCursor
editorCursorToggleDebug = editorCursorDebugL %~ not
