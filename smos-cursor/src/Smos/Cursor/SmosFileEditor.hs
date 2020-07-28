{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.SmosFileEditor where

import Control.DeepSeq
import qualified Data.List.NonEmpty as NE
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import Smos.Cursor.SmosFile
import Smos.Data
import Smos.History

data SmosFileEditorCursor
  = SmosFileEditorCursor
      { smosFileEditorCursorPath :: !(Path Abs File),
        smosFileEditorCursorHistory :: !(History (Maybe SmosFileCursor)) -- Nothing means an empty smos file, s othe Maybe needs to be in the history.
      }
  deriving (Show, Eq, Generic)

instance Validity SmosFileEditorCursor

instance NFData SmosFileEditorCursor

makeSmosFileEditorCursor :: Path Abs File -> SmosFile -> SmosFileEditorCursor
makeSmosFileEditorCursor p sf =
  SmosFileEditorCursor
    { smosFileEditorCursorHistory = startingHistory $ makeSmosFileCursor <$> NE.nonEmpty (smosFileForest sf),
      smosFileEditorCursorPath = p
    }

rebuildSmosFileEditorCursor :: SmosFileEditorCursor -> (Path Abs File, SmosFile)
rebuildSmosFileEditorCursor SmosFileEditorCursor {..} =
  ( smosFileEditorCursorPath,
    maybe emptySmosFile rebuildSmosFileCursorEntirely $ historyPresent smosFileEditorCursorHistory
  )
