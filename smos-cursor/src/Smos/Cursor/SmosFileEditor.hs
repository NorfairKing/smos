{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.SmosFileEditor where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Function
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Time
import GHC.Generics (Generic)
import Lens.Micro
import Path
import Path.IO
import Smos.Cursor.SmosFile
import Smos.Data
import Smos.History
import System.FileLock
import UnliftIO.Resource

-- This represents a SmosFile on disk
data SmosFileEditorCursor = SmosFileEditorCursor
  { smosFileEditorPath :: !(Path Abs File),
    smosFileEditorStartingPoint :: !(Maybe SmosFile), -- Nothing means it was an empty file
    smosFileEditorCursorHistory :: !(History (Maybe SmosFileCursor)), -- Nothing means an empty smos file, s othe Maybe needs to be in the history.
    smosFileEditorUnsavedChanges :: !Bool, -- Whether any changes have been made since the 'last saved' or since the beginning
    smosFileEditorLastSaved :: !UTCTime, -- Starts with the opening of the file
    smosFileEditorLock :: !FileLock, -- The file lock that proves we have a lock on the file. We also need it to close it at the end but we do that using:
    smosFileEditorReleaseKey :: !ReleaseKey -- The key to release the file lock early
  }
  deriving (Generic)

-- | Left if there was a problem while reading
startSmosFileEditorCursor :: MonadResource m => Path Abs File -> m (Maybe (Either String SmosFileEditorCursor))
startSmosFileEditorCursor p = do
  (rk, mfl) <- tryLockSmosFile p
  forM mfl $ \fl -> do
    mErrOrSF <- liftIO $ readSmosFile p
    let errOrStartingPoint = case mErrOrSF of
          Nothing -> Right Nothing
          Just errOrSF -> Just <$> errOrSF
    forM errOrStartingPoint $ \startingPoint -> do
      now <- liftIO getCurrentTime
      pure
        SmosFileEditorCursor
          { smosFileEditorPath = p,
            smosFileEditorStartingPoint = startingPoint,
            smosFileEditorCursorHistory = startingHistory $ smosFileCursorReadyForStartup . makeSmosFileCursor <$> (startingPoint >>= NE.nonEmpty . smosFileForest),
            smosFileEditorUnsavedChanges = isNothing startingPoint, -- Because we'll be editing an empty file, not a nonexistent file
            smosFileEditorLastSaved = now,
            smosFileEditorLock = fl,
            smosFileEditorReleaseKey = rk
          }

-- TODO do something if the contents on disk have changed instead of just overwriting
startSmosFileEditorCursorWithCursor :: MonadResource m => Path Abs File -> Maybe SmosFileCursor -> m (Maybe SmosFileEditorCursor)
startSmosFileEditorCursorWithCursor p msfc = do
  (rk, mfl) <- tryLockSmosFile p
  forM mfl $ \fl -> do
    now <- liftIO getCurrentTime
    pure
      SmosFileEditorCursor
        { smosFileEditorPath = p,
          smosFileEditorStartingPoint = rebuildSmosFileCursorEntirely <$> msfc,
          smosFileEditorCursorHistory = startingHistory msfc,
          smosFileEditorUnsavedChanges = False,
          smosFileEditorLastSaved = now,
          smosFileEditorLock = fl,
          smosFileEditorReleaseKey = rk
        }

resetSmosFileEditorCursor :: Maybe SmosFileCursor -> SmosFileEditorCursor -> SmosFileEditorCursor
resetSmosFileEditorCursor msfc sfc =
  sfc
    { smosFileEditorStartingPoint = rebuildSmosFileCursorEntirely <$> msfc,
      smosFileEditorCursorHistory = startingHistory msfc,
      smosFileEditorUnsavedChanges = ((/=) `on` fmap rebuildSmosFileCursorEntirely) msfc (smosFileEditorCursorPresent sfc)
    }

tryLockSmosFile :: MonadResource m => Path Abs File -> m (ReleaseKey, Maybe FileLock)
tryLockSmosFile p = do
  ensureDir (parent p)
  lockFilePath <- liftIO $ lockFilePathFor p
  allocate
    (tryLockFile (fromAbsFile lockFilePath) Exclusive) -- We will edit the file so we need an exclusive lock
    ( \mfl ->
        forM_ mfl $ \fl -> do
          ignoringAbsence $ removeFile lockFilePath
          unlockFile fl
    )

lockFilePathFor :: MonadThrow m => Path Abs File -> m (Path Abs File)
lockFilePathFor p = do
  p' <- addExtension ".lock" p
  let par = parent p'
      fn = filename p'
  fn' <- parseRelFile $ "." <> fromRelFile fn
  pure $ par </> fn'

-- | The cursor should be considered invalidated after this
--
-- It closes the lock but _does not_ save the file
smosFileEditorCursorClose :: MonadResource m => SmosFileEditorCursor -> m ()
smosFileEditorCursorClose SmosFileEditorCursor {..} = release smosFileEditorReleaseKey

smosFileEditorCursorSave :: MonadIO m => SmosFileEditorCursor -> m SmosFileEditorCursor
smosFileEditorCursorSave sfec@SmosFileEditorCursor {..} = do
  let sf' = rebuildSmosFileEditorCursor sfec
  liftIO $ saveSmosFile sf' smosFileEditorStartingPoint smosFileEditorPath
  now <- liftIO getCurrentTime
  pure $
    sfec
      { smosFileEditorUnsavedChanges = False,
        smosFileEditorLastSaved = now,
        smosFileEditorStartingPoint = Just sf'
      }

saveSmosFile :: SmosFile -> Maybe SmosFile -> Path Abs File -> IO ()
saveSmosFile sf' smosStateStartSmosFile smosStateFilePath = do
  e <- doesFileExist smosStateFilePath
  when (e && isNothing smosStateStartSmosFile) $ removeFile smosStateFilePath
  ( case smosStateStartSmosFile of
      Nothing -> unless (sf' == emptySmosFile)
      Just sf'' -> unless (sf'' == sf')
    )
    $ do
      ensureDir $ parent smosStateFilePath
      writeSmosFile smosStateFilePath sf'

rebuildSmosFileEditorCursor :: SmosFileEditorCursor -> SmosFile
rebuildSmosFileEditorCursor SmosFileEditorCursor {..} =
  maybe emptySmosFile rebuildSmosFileCursorEntirely $ historyPresent smosFileEditorCursorHistory

smosFileEditorCursorHistoryL :: Lens' SmosFileEditorCursor (History (Maybe SmosFileCursor))
smosFileEditorCursorHistoryL = lens smosFileEditorCursorHistory $ \sfec h -> sfec {smosFileEditorCursorHistory = h}

smosFileEditorCursorPresent :: SmosFileEditorCursor -> Maybe SmosFileCursor
smosFileEditorCursorPresent = historyPresent . smosFileEditorCursorHistory

smosFileEditorCursorUpdateTime :: ZonedTime -> SmosFileEditorCursor -> SmosFileEditorCursor
smosFileEditorCursorUpdateTime zt = smosFileEditorCursorHistoryL . historyPresentL %~ fmap (smosFileCursorUpdateTime zt)
