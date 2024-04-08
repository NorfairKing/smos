{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Archive.Commands.File
  ( smosArchiveFile,

    -- ** Helper functions
    determineToFile,
    NotInWorkflowDir (..),
    destinationFile,
    archiveTimeFormat,
    checkFromFile,
    ArchiveCheckResult (..),
    dealWithArchiveCheckResult,
    moveToArchive,
    ArchiveMoveResult (..),
    dealWithArchiveMoveResult,
    prepareToArchive,
  )
where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Tree
import Path
import Path.IO
import Smos.Archive.Env
import Smos.Archive.OptParse
import Smos.CLI.Prompt
import Smos.Data
import Smos.Directory.Resolution
import System.Exit
import qualified System.FilePath as FP

smosArchiveFile :: Path Abs File -> A ()
smosArchiveFile from = do
  to <- determineToFile from
  liftIO $ do
    sf <- checkFromFile from >>= dealWithArchiveCheckResult from
    moveToArchive from to sf >>= dealWithArchiveMoveResult

determineToFile :: (MonadIO m, MonadReader Settings m) => Path Abs File -> m (Path Abs File)
determineToFile file = do
  workflowDir <- asks (resolveDirWorkflowDir . setDirectorySettings) >>= liftIO
  archiveDir <- asks (resolveDirArchiveDir . setDirectorySettings) >>= liftIO
  lt <- liftIO getLocalTime
  liftIO $ destinationFile lt workflowDir archiveDir file

destinationFile ::
  (MonadThrow m) =>
  LocalTime ->
  -- | Workflow dir
  Path Abs Dir ->
  -- | Archive dir
  Path Abs Dir ->
  -- | File to archive
  Path Abs File ->
  -- | Archive file
  m (Path Abs File)
destinationFile lt workflowDir archiveDir file = do
  case stripProperPrefix workflowDir file of
    Nothing -> throwM (NotInWorkflowDir workflowDir file)
    Just rf -> do
      let mext = fileExtension rf :: Maybe String
      let withoutExt = FP.dropExtensions (fromRelFile rf)
      let newRelFile = withoutExt ++ "_" ++ formatTime defaultTimeLocale archiveTimeFormat lt
      arf' <- parseRelFile newRelFile
      arf'' <- maybe (pure arf') (`replaceExtension` arf') mext
      pure $ archiveDir </> arf''

archiveTimeFormat :: String
archiveTimeFormat = "%F_%H%M%S"

data NotInWorkflowDir = NotInWorkflowDir (Path Abs Dir) (Path Abs File)
  deriving (Show)

instance Exception NotInWorkflowDir where
  displayException (NotInWorkflowDir workflowDir file) =
    unlines
      [ "The smos file",
        fromAbsFile file,
        "is not in the smos workflow directory",
        fromAbsDir workflowDir
      ]

data ArchiveCheckResult
  = CheckFileToArchiveDoesNotExist
  | CheckNotASmosFile String
  | NotAllDone SmosFile
  | ReadyToArchive SmosFile

checkFromFile :: Path Abs File -> IO ArchiveCheckResult
checkFromFile from = do
  mErrOrSF <- readSmosFile from
  pure $ case mErrOrSF of
    Nothing -> CheckFileToArchiveDoesNotExist
    Just (Left err) -> CheckNotASmosFile err
    Just (Right sf) ->
      let allDone =
            all
              (all (maybe True todoStateIsDone . entryState) . flatten)
              (smosFileForest sf)
       in if allDone
            then ReadyToArchive sf
            else NotAllDone sf

dealWithArchiveCheckResult :: Path Abs File -> ArchiveCheckResult -> IO SmosFile
dealWithArchiveCheckResult from = \case
  ReadyToArchive sf -> pure sf
  CheckFileToArchiveDoesNotExist -> die $ unwords ["File does not exist:", fromAbsFile from]
  CheckNotASmosFile err -> die $ unlines [unwords ["The file to archive doesn't look like a smos file:", fromAbsFile from], err]
  NotAllDone sf -> do
    res <-
      promptYesNo No $
        T.pack $
          unlines
            [ unwords ["Not all entries in", fromAbsFile from, "are done."],
              "Are you sure that you want to archive it?",
              "All remaining non-done entries will be set to CANCELLED."
            ]
    case res of
      Yes -> pure sf
      No -> die "Not archiving."

data ArchiveMoveResult
  = MoveDestinationAlreadyExists (Path Abs File)
  | ArchivedSuccesfully

moveToArchive :: (MonadIO m) => Path Abs File -> Path Abs File -> SmosFile -> m ArchiveMoveResult
moveToArchive from to sf = do
  ensureDir $ parent to
  e2 <- doesFileExist to
  if e2
    then pure $ MoveDestinationAlreadyExists to
    else do
      now <- liftIO getCurrentTime
      let archivedSmosFile = prepareToArchive now sf
      liftIO $ writeSmosFile to archivedSmosFile
      removeFile from
      pure ArchivedSuccesfully

dealWithArchiveMoveResult :: ArchiveMoveResult -> IO ()
dealWithArchiveMoveResult = \case
  ArchivedSuccesfully -> pure ()
  MoveDestinationAlreadyExists dest -> die $ unlines ["The destination file already exists:", fromAbsFile dest]

prepareToArchive :: UTCTime -> SmosFile -> SmosFile
prepareToArchive now = smosFileClockOutEverywhere now . setAllUndoneToCancelled now

setAllUndoneToCancelled :: UTCTime -> SmosFile -> SmosFile
setAllUndoneToCancelled now sf = makeSmosFile $ map (fmap go) (smosFileForest sf)
  where
    go :: Entry -> Entry
    go e = case entryState e of
      Nothing -> e
      Just s ->
        if todoStateIsDone s
          then e
          else fromMaybe e $ entrySetState now (Just "CANCELLED") e
