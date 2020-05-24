{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Editor.Gen where

import Cursor.DirForest.Gen ()
import Data.GenValidity
import Smos.Cursor.FileBrowser.Gen ()
import Smos.Cursor.Help.Gen ()
import Smos.Cursor.Report.Next.Gen ()
import Smos.Cursor.SmosFile.Gen ()
import Smos.Types

instance GenValid EditorCursor where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ReportCursor where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid EditorSelection where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
