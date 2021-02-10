{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.FileBrowser where

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Cursor.FileOrDir
import Cursor.Simple.DirForest
import Cursor.Text
import Cursor.Types
import Data.DirForest (DirForest (..))
import qualified Data.DirForest as DF
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Path
import Path.IO
import Smos.Archive as Archive
import Smos.Data
import Smos.Undo

data FileBrowserCursor = FileBrowserCursor
  { fileBrowserCursorBase :: Path Abs Dir,
    fileBrowserCursorDirForest :: !(DirForest ()),
    fileBrowserCursorDirForestCursor :: !(Maybe (DirForestCursor ())), -- Nothing means there are no files to display
    fileBrowserCursorUndoStack :: !(UndoStack FileBrowserCursorAction),
    fileBrowserCursorFilterBar :: !TextCursor,
    fileBrowserCursorSelection :: !FileBrowserCursorSelection
  }
  deriving (Show, Eq, Generic)

instance Validity FileBrowserCursor

instance NFData FileBrowserCursor

data FileBrowserCursorSelection = FileBrowserSelected | FileBrowserFilterSelected
  deriving (Show, Eq, Generic)

instance Validity FileBrowserCursorSelection

instance NFData FileBrowserCursorSelection

makeFileBrowserCursor :: Path Abs Dir -> DirForest () -> FileBrowserCursor
makeFileBrowserCursor base df =
  FileBrowserCursor
    { fileBrowserCursorBase = base,
      fileBrowserCursorDirForest = df,
      fileBrowserCursorDirForestCursor = makeDirForestCursor df,
      fileBrowserCursorUndoStack = emptyUndoStack,
      fileBrowserCursorFilterBar = emptyTextCursor,
      fileBrowserCursorSelection = FileBrowserSelected
    }

rebuildFileBrowserCursor :: FileBrowserCursor -> DirForest ()
rebuildFileBrowserCursor FileBrowserCursor {..} = maybe DF.empty (fromMaybe DF.empty . dullDelete . rebuildDirForestCursor) fileBrowserCursorDirForestCursor

fileBrowserCursorDirForestCursorL :: Lens' FileBrowserCursor (Maybe (DirForestCursor ()))
fileBrowserCursorDirForestCursorL = lens fileBrowserCursorDirForestCursor $ \fbc fc -> fbc {fileBrowserCursorDirForestCursor = fc}

fileBrowserCursorSelectionL :: Lens' FileBrowserCursor FileBrowserCursorSelection
fileBrowserCursorSelectionL = lens fileBrowserCursorSelection (\narc cs -> narc {fileBrowserCursorSelection = cs})

fileBrowserCursorFilterBarL :: Lens' FileBrowserCursor TextCursor
fileBrowserCursorFilterBarL = lens fileBrowserCursorFilterBar $ \fbc@FileBrowserCursor {..} tc ->
  let query = rebuildTextCursor fileBrowserCursorFilterBar
      filteredSelection = do
        dfc <- fileBrowserCursorDirForestCursor
        let filteredDF = DF.filterWithKey (\rf _ -> T.unpack query `isInfixOf` fromRelFile rf) . fromMaybe DF.empty . dullDelete $ rebuildDirForestCursor dfc
        prunedDF <- DF.pruneEmptyDirs filteredDF
        dfc' <- makeDirForestCursor prunedDF
        let goDowntoFile c =
              let (_, fodC) = dirForestCursorSelected c
               in case fodC of
                    InProgress _ -> c -- should not happen, but fine.
                    Existent (FodFile _ _) -> c -- It's a file, stay here.
                    Existent (FodDir _) -> case dirForestCursorOpen c >>= (join . dullDelete . dirForestCursorSelectNext) of -- It's a dir, probably not interesting, recurse downwards
                      Nothing -> c -- Done, stay here.
                      Just c' -> goDowntoFile c'
        pure $ goDowntoFile dfc'
   in fbc
        { fileBrowserCursorFilterBar = tc,
          fileBrowserCursorDirForestCursor = filteredSelection
        }

fileBrowserRecordMovementHistory :: FileBrowserCursor -> Maybe (DirForestCursor ()) -> FileBrowserCursor
fileBrowserRecordMovementHistory fbc new =
  fbc
    { fileBrowserCursorDirForestCursor = new,
      fileBrowserCursorUndoStack = undoStackPush (Movement new) (fileBrowserCursorUndoStack fbc)
    }

-- Start with a possibly empty dirforest cursor, possibly fail
fileBrowserCursorMaybeDoMaybe :: (Maybe (DirForestCursor ()) -> Maybe (DirForestCursor ())) -> FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorMaybeDoMaybe func fbc = do
  dfc' <- func $ fileBrowserCursorDirForestCursor fbc
  pure $ fileBrowserRecordMovementHistory fbc (Just dfc')

fileBrowserCursorDoMaybe :: (DirForestCursor () -> Maybe (DirForestCursor ())) -> FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorDoMaybe func = fileBrowserCursorMaybeDoMaybe (>>= func)

fileBrowserCursorDoDeleteOrUpdate :: (DirForestCursor () -> DeleteOrUpdate (DirForestCursor ())) -> FileBrowserCursor -> FileBrowserCursor
fileBrowserCursorDoDeleteOrUpdate func fbc =
  case fileBrowserCursorDirForestCursor fbc of
    Nothing -> fbc
    Just dfc -> fileBrowserRecordMovementHistory fbc $
      case func dfc of
        Deleted -> Nothing
        Updated dfc' -> Just dfc'

fileBrowserCursorDoDeleteOrUpdateMaybe :: (DirForestCursor () -> DeleteOrUpdate (Maybe (DirForestCursor ()))) -> FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorDoDeleteOrUpdateMaybe func fbc = case fileBrowserCursorDirForestCursor fbc of
  Nothing -> Just fbc
  Just dfc ->
    case func dfc of
      Updated Nothing -> Nothing
      Deleted -> Just $ fileBrowserRecordMovementHistory fbc Nothing
      Updated (Just dfc') -> Just $ fileBrowserRecordMovementHistory fbc $ Just dfc'

fileBrowserCursorDoMaybeDeleteOrUpdate :: (DirForestCursor () -> Maybe (DeleteOrUpdate (DirForestCursor ()))) -> FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorDoMaybeDeleteOrUpdate func fbc = case fileBrowserCursorDirForestCursor fbc of
  Nothing -> Just fbc
  Just dfc ->
    case func dfc of
      Nothing -> Nothing
      Just Deleted -> Just $ fileBrowserRecordMovementHistory fbc Nothing
      Just (Updated dfc') -> Just $ fileBrowserRecordMovementHistory fbc $ Just dfc'

fileBrowserCursorSelectNext :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorSelectNext = fileBrowserCursorDoDeleteOrUpdateMaybe dirForestCursorSelectNext

fileBrowserCursorSelectPrev :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorSelectPrev = fileBrowserCursorDoDeleteOrUpdateMaybe dirForestCursorSelectPrev

fileBrowserCursorSelectFirst :: FileBrowserCursor -> FileBrowserCursor
fileBrowserCursorSelectFirst = fileBrowserCursorDoDeleteOrUpdate dirForestCursorSelectFirst

fileBrowserCursorSelectLast :: FileBrowserCursor -> FileBrowserCursor
fileBrowserCursorSelectLast = fileBrowserCursorDoDeleteOrUpdate dirForestCursorSelectLast

fileBrowserCursorStartNew :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorStartNew = fileBrowserCursorMaybeDoMaybe dirForestCursorStartNew

fileBrowserCursorStartNewBelowAtStart :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorStartNewBelowAtStart = fileBrowserCursorDoMaybe dirForestCursorStartNewBelowAtStart

fileBrowserCursorStartNewBelowAtEnd :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorStartNewBelowAtEnd = fileBrowserCursorDoMaybe dirForestCursorStartNewBelowAtEnd

fileBrowserCursorStopNew :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorStopNew = fileBrowserCursorDoMaybeDeleteOrUpdate dirForestCursorStopNew

fileBrowserCursorInsertChar :: Char -> FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorInsertChar c = fileBrowserCursorDoMaybe $ dirForestCursorInsertChar c

fileBrowserCursorAppendChar :: Char -> FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorAppendChar c = fileBrowserCursorDoMaybe $ dirForestCursorAppendChar c

fileBrowserCursorRemoveChar :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorRemoveChar = fileBrowserCursorDoMaybeDeleteOrUpdate dirForestCursorRemoveChar

fileBrowserCursorDeleteChar :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorDeleteChar = fileBrowserCursorDoMaybeDeleteOrUpdate dirForestCursorDeleteChar

fileBrowserCursorSelectPrevChar :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorSelectPrevChar = fileBrowserCursorDoMaybe dirForestCursorSelectPrevChar

fileBrowserCursorSelectNextChar :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorSelectNextChar = fileBrowserCursorDoMaybe dirForestCursorSelectNextChar

fileBrowserCursorToggle :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorToggle = fileBrowserCursorDoMaybe dirForestCursorToggle

fileBrowserCursorToggleRecursively :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorToggleRecursively = fileBrowserCursorDoMaybe dirForestCursorToggleRecursively

fileBrowserCursorSelectBrowser :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorSelectBrowser = fileBrowserCursorSelectionL $
  \case
    FileBrowserSelected -> Nothing
    FileBrowserFilterSelected -> Just FileBrowserSelected

fileBrowserCursorSelectFilter :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorSelectFilter = fileBrowserCursorSelectionL $
  \case
    FileBrowserFilterSelected -> Nothing
    FileBrowserSelected -> Just FileBrowserFilterSelected

fileBrowserCursorFilterInsertChar :: Char -> FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorFilterInsertChar c = fileBrowserCursorFilterBarL $ textCursorInsert c

fileBrowserCursorFilterAppendChar :: Char -> FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorFilterAppendChar c = fileBrowserCursorFilterBarL $ textCursorAppend c

fileBrowserCursorFilterRemoveChar :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorFilterRemoveChar =
  fileBrowserCursorFilterBarL $
    \tc ->
      case textCursorRemove tc of
        Nothing -> Nothing
        Just Deleted -> Nothing
        Just (Updated narc) -> Just narc

fileBrowserCursorFilterDeleteChar :: FileBrowserCursor -> Maybe FileBrowserCursor
fileBrowserCursorFilterDeleteChar =
  fileBrowserCursorFilterBarL $
    \tc ->
      case textCursorDelete tc of
        Nothing -> Nothing
        Just Deleted -> Nothing
        Just (Updated narc) -> Just narc

fileBrowserSelected :: FileBrowserCursor -> Maybe (Path Abs Dir, Path Rel Dir, FileOrDirCursor ())
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
            InProgress _ -> pure fbc
            Existent (FodFile _ _) -> pure fbc
            Existent (FodDir rdd) -> do
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

fileBrowserArchiveFile :: MonadIO m => Path Abs Dir -> Path Abs Dir -> FileBrowserCursor -> m FileBrowserCursor
fileBrowserArchiveFile workflowDir archiveDir fbc =
  case fileBrowserCursorDirForestCursor fbc of
    Nothing -> pure fbc
    Just dfc -> do
      let (rd, fod) = dirForestCursorSelected dfc
      case fod of
        InProgress _ -> pure fbc
        Existent (FodDir _) -> pure fbc
        Existent (FodFile rp _) -> do
          let src = fileBrowserCursorBase fbc </> rd </> rp
          lt <- liftIO getLocalTime
          case destinationFile lt workflowDir archiveDir src of
            Nothing -> pure fbc
            Just dest -> do
              acr <- liftIO $ checkFromFile src
              let goOn sf = do
                    let a = ArchiveSmosFile src dest sf dfc
                    let us' = undoStackPush a (fileBrowserCursorUndoStack fbc)
                    r <- redoArchiveFile src dest sf
                    pure $ case r of
                      MoveDestinationAlreadyExists _ -> fbc
                      ArchivedSuccesfully ->
                        fbc
                          { fileBrowserCursorDirForestCursor = dullDelete $ dirForestCursorDeleteCurrent dfc,
                            fileBrowserCursorUndoStack = us'
                          }
              case acr of
                ReadyToArchive sf -> goOn sf
                NotAllDone sf -> goOn sf
                _ -> pure fbc

fileBrowserCompleteToDir :: MonadIO m => FileBrowserCursor -> m FileBrowserCursor
fileBrowserCompleteToDir fbc =
  case fileBrowserCursorDirForestCursor fbc of
    Nothing -> pure fbc
    Just dfc ->
      let (rd1, _) = dirForestCursorSelected dfc
       in case dirForestCursorCompleteToDir dfc of
            Nothing -> pure fbc
            Just (rd2, dfc') -> do
              let dir = fileBrowserCursorBase fbc </> rd1 </> rd2
              let a = DirCreation dir dfc
              let us' = undoStackPush a (fileBrowserCursorUndoStack fbc)
              redoDirCreation dir
              pure $
                fbc
                  { fileBrowserCursorDirForestCursor = Just dfc',
                    fileBrowserCursorUndoStack = us'
                  }

fileBrowserCompleteToFile :: MonadIO m => FileBrowserCursor -> m FileBrowserCursor
fileBrowserCompleteToFile fbc =
  case fileBrowserCursorDirForestCursor fbc of
    Nothing -> pure fbc
    Just dfc ->
      let (rd, _) = dirForestCursorSelected dfc
       in case dirForestCursorCompleteToFile' (replaceExtension ".smos") () dfc of
            Nothing -> pure fbc
            Just (rf, dfc') -> do
              let file = fileBrowserCursorBase fbc </> rd </> rf
              let a = FileCreation file dfc
              let us' = undoStackPush a (fileBrowserCursorUndoStack fbc)
              redoFileCreation file
              pure $
                fbc
                  { fileBrowserCursorDirForestCursor = Just dfc',
                    fileBrowserCursorUndoStack = us'
                  }

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
fileBrowserRedoAction a mdfc =
  case a of
    Movement mdfc' -> pure mdfc'
    RmEmptyDir dir dfc' -> redoRmEmptyDir dir >> pure (Just dfc')
    ArchiveSmosFile src dest sf dfc' -> do
      r <- redoArchiveFile src dest sf
      case r of
        MoveDestinationAlreadyExists _ -> pure mdfc
        ArchivedSuccesfully -> pure $ Just dfc'
    DirCreation dp dfc' -> redoDirCreation dp >> pure (Just dfc')
    FileCreation fp dfc' -> redoFileCreation fp >> pure (Just dfc')

fileBrowserUndoAction :: MonadIO m => FileBrowserCursorAction -> Maybe (DirForestCursor ()) -> m (Maybe (DirForestCursor ()))
fileBrowserUndoAction a _ =
  case a of
    Movement mdfc' -> pure mdfc'
    RmEmptyDir dir dfc' -> undoRmEmptyDir dir >> pure (Just dfc')
    ArchiveSmosFile src dest sf dfc' -> undoArchiveFile src dest sf >> pure (Just dfc')
    DirCreation dp dfc' -> undoDirCreation dp >> pure (Just dfc')
    FileCreation fp dfc' -> undoFileCreation fp >> pure (Just dfc')

redoRmEmptyDir :: MonadIO m => Path Abs Dir -> m ()
redoRmEmptyDir dir = do
  mContents <- liftIO $ forgivingAbsence $ listDir dir
  case mContents of
    Nothing -> pure () -- The directory didn't exist
    Just ([], []) -> removeDir dir -- The directory is indeed empty
    Just _ -> pure () -- The directory wasn't empty

undoRmEmptyDir :: MonadIO m => Path Abs Dir -> m ()
undoRmEmptyDir = ensureDir

redoArchiveFile :: MonadIO m => Path Abs File -> Path Abs File -> SmosFile -> m ArchiveMoveResult
redoArchiveFile = Archive.moveToArchive

undoArchiveFile :: MonadIO m => Path Abs File -> Path Abs File -> SmosFile -> m ()
undoArchiveFile src dest sf =
  liftIO $ do
    removeFile dest
    writeSmosFile src sf

redoDirCreation :: MonadIO m => Path Abs Dir -> m ()
redoDirCreation = undoRmEmptyDir

undoDirCreation :: MonadIO m => Path Abs Dir -> m ()
undoDirCreation = redoRmEmptyDir

redoFileCreation :: MonadIO m => Path Abs File -> m ()
redoFileCreation p = liftIO $ writeSmosFile p emptySmosFile

undoFileCreation :: MonadIO m => Path Abs File -> m ()
undoFileCreation = removeFile

data FileBrowserCursorAction
  = -- | The previous version, to reset to
    Movement (Maybe (DirForestCursor ()))
  | -- | The directory that was removed and the previous version to reset to
    RmEmptyDir (Path Abs Dir) (DirForestCursor ())
  | -- The source path,
    -- The destination path
    -- The contents of the file that was archived
    -- The dirforest cursor from before
    ArchiveSmosFile
      (Path Abs File)
      (Path Abs File)
      SmosFile
      (DirForestCursor ())
  | DirCreation (Path Abs Dir) (DirForestCursor ())
  | FileCreation (Path Abs File) (DirForestCursor ())
  deriving (Show, Eq, Generic)

instance Validity FileBrowserCursorAction

instance NFData FileBrowserCursorAction
