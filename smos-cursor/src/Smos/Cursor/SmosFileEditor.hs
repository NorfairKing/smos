{-# LANGUAGE DeriveGeneric #-}

module Smos.Cursor.SmosFileEditor where

import Control.DeepSeq
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import Smos.Cursor.SmosFile
import Smos.History

data SmosFileEditorCursor
  = SmosFileEditorCursor
      { smosFileEditorCursorHistory :: !(History (Maybe SmosFileCursor)), -- Nothing means an empty smos file, s othe Maybe needs to be in the history.
        smosFileEditorCursorPath :: !(Path Abs File)
      }
  deriving (Show, Eq, Generic)

instance Validity SmosFileEditorCursor

instance NFData SmosFileEditorCursor
