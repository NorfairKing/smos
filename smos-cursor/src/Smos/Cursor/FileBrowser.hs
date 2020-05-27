{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.FileBrowser where

import Control.Monad.IO.Class
import Cursor.Simple.DirForest
import Cursor.Types
import Data.DirForest (DirForest (..))
import qualified Data.DirForest as DF
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Path
import Path.IO
import Smos.Undo

data FileBrowserCursor
  = FileBrowserCursor
      { fileBrowserCursorBase :: Path Abs Dir,
        fileBrowserCursorDirForestCursor :: !(Maybe (DirForestCursor ())), -- Nothing means there are no files to display
        fileBrowserCursorUndoStack :: !(UndoStack FileBrowserCursorAction)
      }
  deriving (Show, Eq, Generic)

instance Validity FileBrowserCursor

makeFileBrowserCursor :: Path Abs Dir -> DirForest () -> FileBrowserCursor
makeFileBrowserCursor base df =
  FileBrowserCursor
    { fileBrowserCursorBase = base,
      fileBrowserCursorDirForestCursor = makeDirForestCursor df,
      fileBrowserCursorUndoStack = emptyUndoStack
    }

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
      pure $
        fbc
          { fileBrowserCursorDirForestCursor = Just dfc',
            fileBrowserCursorUndoStack = undoStackPush (Movement (Just dfc)) (fileBrowserCursorUndoStack fbc)
          }

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
  df <- DF.readNonHidden dir (\_ -> pure ())
  pure $ makeFileBrowserCursor dir df

fileBrowserRmEmptyDir :: MonadIO m => FileBrowserCursor -> m FileBrowserCursor
fileBrowserRmEmptyDir fbc =
  case fileBrowserCursorDirForestCursor fbc of
    Nothing -> pure fbc
    Just dfc ->
      let (rd, fod) = dirForestCursorSelected dfc
       in case fod of
            FodFile _ _ -> pure fbc
            FodDir rdd -> do
              let dir = fileBrowserCursorBase fbc </> rd </> rdd
              let a = RmEmptyDir dir dfc
              let us' = undoStackPush a (fileBrowserCursorUndoStack fbc)
              mContents <- liftIO $ forgivingAbsence $ listDir dir
              case mContents of
                Nothing -> pure fbc -- The directory didn't exist
                Just ([], []) -> do
                  -- The directory is indeed empty
                  redoRmEmptyDir dir
                  pure $
                    fbc
                      { fileBrowserCursorDirForestCursor = dullDelete $ dirForestCursorDeleteCurrent dfc,
                        fileBrowserCursorUndoStack = us'
                      }
                _ -> pure fbc -- The dir is not empty, do nothing

-- Fails if there is nothing to undo
fileBrowserUndo :: MonadIO m => FileBrowserCursor -> Maybe (m FileBrowserCursor)
fileBrowserUndo fbc = do
  tup <- undoStackUndo (fileBrowserCursorUndoStack fbc)
  case tup of
    (a, us') ->
      let fbc' = fbc {fileBrowserCursorUndoStack = us'}
       in case a of
            Movement _ -> fileBrowserUndo fbc' -- Just a movement, don't undo it.
            _ -> Just $ do
              mdfc' <- fileBrowserUndoAction a (fileBrowserCursorDirForestCursor fbc)
              pure fbc' {fileBrowserCursorDirForestCursor = mdfc'}

-- Fails if there is nothing to undo
fileBrowserUndoAny :: MonadIO m => FileBrowserCursor -> Maybe (m FileBrowserCursor)
fileBrowserUndoAny fbc = do
  tup <- undoStackUndo (fileBrowserCursorUndoStack fbc)
  case tup of
    (a, us') -> Just $ do
      let fbc' = fbc {fileBrowserCursorUndoStack = us'}
      mdfc' <- fileBrowserUndoAction a (fileBrowserCursorDirForestCursor fbc)
      pure fbc' {fileBrowserCursorDirForestCursor = mdfc'}

-- Fails if there is nothing to undo
fileBrowserRedo :: MonadIO m => FileBrowserCursor -> Maybe (m FileBrowserCursor)
fileBrowserRedo fbc = do
  tup <- undoStackRedo (fileBrowserCursorUndoStack fbc)
  case tup of
    (a, us') ->
      let fbc' = fbc {fileBrowserCursorUndoStack = us'}
       in case a of
            Movement _ -> fileBrowserRedo fbc' -- Just a movement, don't redo it.
            _ -> Just $ do
              mdfc' <- fileBrowserRedoAction a (fileBrowserCursorDirForestCursor fbc)
              pure fbc' {fileBrowserCursorDirForestCursor = mdfc'}

-- Fails if there is nothing to redo
fileBrowserRedoAny :: MonadIO m => FileBrowserCursor -> Maybe (m FileBrowserCursor)
fileBrowserRedoAny fbc = do
  tup <- undoStackRedo (fileBrowserCursorUndoStack fbc)
  case tup of
    (a, us') -> Just $ do
      let fbc' = fbc {fileBrowserCursorUndoStack = us'}
      mdfc' <- fileBrowserRedoAction a (fileBrowserCursorDirForestCursor fbc)
      pure fbc' {fileBrowserCursorDirForestCursor = mdfc'}

fileBrowserRedoAction :: MonadIO m => FileBrowserCursorAction -> Maybe (DirForestCursor ()) -> m (Maybe (DirForestCursor ()))
fileBrowserRedoAction a _ =
  case a of
    Movement mdfc' -> pure mdfc'
    RmEmptyDir dir dfc' -> do
      redoRmEmptyDir dir
      pure $ Just dfc'

fileBrowserUndoAction :: MonadIO m => FileBrowserCursorAction -> Maybe (DirForestCursor ()) -> m (Maybe (DirForestCursor ()))
fileBrowserUndoAction a _ =
  case a of
    Movement mdfc' -> pure mdfc'
    RmEmptyDir dir dfc' -> do
      undoRmEmptyDir dir
      pure $ Just dfc'

redoRmEmptyDir :: MonadIO m => Path Abs Dir -> m ()
redoRmEmptyDir dir = do
  mContents <- liftIO $ forgivingAbsence $ listDir dir
  case mContents of
    Nothing -> pure () -- The directory didn't exist
    Just ([], []) -> removeDir dir -- The directory is indeed empty
    Just _ -> pure () -- The directory wasn't empty

undoRmEmptyDir :: MonadIO m => Path Abs Dir -> m ()
undoRmEmptyDir = ensureDir

data FileBrowserCursorAction
  = -- | The previous version, to reset to
    Movement (Maybe (DirForestCursor ()))
  | -- | The directory that was removed and the previous version to reset to
    RmEmptyDir (Path Abs Dir) (DirForestCursor ())
  deriving (Show, Eq, Generic)

instance Validity FileBrowserCursorAction
