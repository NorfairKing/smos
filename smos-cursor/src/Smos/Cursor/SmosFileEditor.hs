{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.SmosFileEditor where

import Control.DeepSeq
import qualified Data.List.NonEmpty as NE
import Data.Time
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Lens.Micro
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
makeSmosFileEditorCursor p sf = makeSmosFileEditorCursorFromCursor p $ makeSmosFileCursor <$> NE.nonEmpty (smosFileForest sf)

makeSmosFileEditorCursorFromCursor :: Path Abs File -> Maybe SmosFileCursor -> SmosFileEditorCursor
makeSmosFileEditorCursorFromCursor p msfc =
  SmosFileEditorCursor
    { smosFileEditorCursorHistory = startingHistory msfc,
      smosFileEditorCursorPath = p
    }

rebuildSmosFileEditorCursor :: SmosFileEditorCursor -> (Path Abs File, SmosFile)
rebuildSmosFileEditorCursor SmosFileEditorCursor {..} =
  ( smosFileEditorCursorPath,
    maybe emptySmosFile rebuildSmosFileCursorEntirely $ historyPresent smosFileEditorCursorHistory
  )

smosFileEditorCursorPathL :: Lens' SmosFileEditorCursor (Path Abs File)
smosFileEditorCursorPathL = lens smosFileEditorCursorPath $ \sfec p -> sfec {smosFileEditorCursorPath = p}

smosFileEditorCursorHistoryL :: Lens' SmosFileEditorCursor (History (Maybe SmosFileCursor))
smosFileEditorCursorHistoryL = lens smosFileEditorCursorHistory $ \sfec h -> sfec {smosFileEditorCursorHistory = h}

smosFileEditorCursorPresent :: SmosFileEditorCursor -> Maybe SmosFileCursor
smosFileEditorCursorPresent = historyPresent . smosFileEditorCursorHistory

smosFileEditorCursorUpdateTime :: ZonedTime -> SmosFileEditorCursor -> SmosFileEditorCursor
smosFileEditorCursorUpdateTime zt = smosFileEditorCursorHistoryL . historyPresentL %~ fmap (smosFileCursorUpdateTime zt)
