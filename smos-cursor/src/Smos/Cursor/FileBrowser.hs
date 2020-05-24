{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.FileBrowser where

import Control.Monad.IO.Class
import Cursor.Simple.DirForest
import Data.DirForest (DirForest (..))
import qualified Data.DirForest as DF
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Path

data FileBrowserCursor
  = FileBrowserCursor
      { fileBrowserCursorBase :: Path Abs Dir,
        fileBrowserCursorDirForestCursor :: !(Maybe (DirForestCursor ())) -- Nothing means there are no files to display
      }
  deriving (Show, Eq, Generic)

instance Validity FileBrowserCursor

makeFileBrowserCursor :: Path Abs Dir -> DirForest () -> FileBrowserCursor
makeFileBrowserCursor base df = FileBrowserCursor {fileBrowserCursorBase = base, fileBrowserCursorDirForestCursor = makeDirForestCursor df}

rebuildFileBrowserCursor :: FileBrowserCursor -> DirForest ()
rebuildFileBrowserCursor FileBrowserCursor {..} = maybe DF.empty rebuildDirForestCursor fileBrowserCursorDirForestCursor

fileBrowserCursorDirForestCursorL :: Lens' FileBrowserCursor (Maybe (DirForestCursor ()))
fileBrowserCursorDirForestCursorL = lens fileBrowserCursorDirForestCursor $ \fbc fc -> fbc {fileBrowserCursorDirForestCursor = fc}

fileBrowserCursorDoMaybe :: (DirForestCursor () -> Maybe (DirForestCursor ())) -> FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorDoMaybe func fbc =
  case fileBrowserCursorDirForestCursor fbc of
    Nothing -> Just fbc
    Just dfc -> do
      dfc' <- func dfc
      pure $ fbc {fileBrowserCursorDirForestCursor = Just dfc'}

fileBrowserCursorSelectNext :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorSelectNext = fileBrowserCursorDoMaybe dirForestCursorSelectNext

fileBrowserCursorSelectPrev :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorSelectPrev = fileBrowserCursorDoMaybe dirForestCursorSelectPrev

fileBrowserCursorToggle :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorToggle = fileBrowserCursorDoMaybe dirForestCursorToggle

fileBrowserCursorToggleRecursively :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorToggleRecursively = fileBrowserCursorDoMaybe dirForestCursorToggleRecursively

fileBrowserSelected :: FileBrowserCursor -> Maybe (Path Abs Dir, Path Rel Dir, FileOrDir ())
fileBrowserSelected FileBrowserCursor {..} = do
  (rd, fod) <- dirForestCursorSelected <$> fileBrowserCursorDirForestCursor
  pure (fileBrowserCursorBase, rd, fod)

startFileBrowserCursor :: MonadIO m => Path Abs Dir -> m FileBrowserCursor
startFileBrowserCursor dir = do
  let filePred fp = fileExtension fp == ".smos"
      dirPred = const True
  df <- DF.readNonHiddenFiltered filePred dirPred dir (\_ -> pure ())
  pure $ makeFileBrowserCursor dir df
