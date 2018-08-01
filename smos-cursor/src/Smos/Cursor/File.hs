module Smos.Cursor.File where

import Data.Validity

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Cursor.Forest

import Smos.Data.Types

import Smos.Cursor.Entry

type FileCursor = ForestCursor EntryCursor

makeSmosFileCursor :: NonEmpty (Tree Entry) -> FileCursor
makeSmosFileCursor = makeForestCursor . NE.map (fmap makeEntryCursor)

rebuildSmosFileCursor :: FileCursor -> NonEmpty (Tree Entry)
rebuildSmosFileCursor = NE.map (fmap rebuildEntryCursor) . rebuildForestCursor
