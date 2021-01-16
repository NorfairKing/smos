{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Archive
  ( smosArchive,

    -- ** Helper functions
    determineToFile,
    NotInWorkflowDir (..),
    destinationFile,
    checkFromFile,
    ArchiveCheckResult (..),
    dealWithArchiveCheckResult,
    moveToArchive,
    ArchiveMoveResult (..),
    dealWithArchiveMoveResult,
    isDone,
    prepareToArchive,
  )
where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Maybe
import Data.Time
import Data.Tree
import Path
import Path.IO
import Smos.Archive.OptParse
import Smos.Archive.Prompt
import Smos.Data
import Smos.Report.Config
import System.Exit
import qualified System.FilePath as FP

smosArchive :: IO ()
smosArchive = do
  Settings {..} <- getSettings
  runReaderT (archive setFile) setDirectorySettings

type Q a = ReaderT DirectoryConfig IO a

archive :: Path Abs File -> Q ()
archive from = do
  to <- determineToFile from
  liftIO $ do
    sf <- checkFromFile from >>= dealWithArchiveCheckResult from
    moveToArchive from to sf >>= dealWithArchiveMoveResult

determineToFile :: Path Abs File -> Q (Path Abs File)
determineToFile file = do
  workflowDir <- asks resolveDirWorkflowDir >>= liftIO
  archiveDir <- asks resolveDirArchiveDir >>= liftIO
  lt <- liftIO getLocalTime
  destinationFile lt workflowDir archiveDir file

destinationFile ::
  MonadThrow m =>
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
      let newRelFile = withoutExt ++ "_" ++ formatTime defaultTimeLocale "%F_%T" lt
      arf' <- parseRelFile newRelFile
      arf'' <- maybe (pure arf') (`replaceExtension` arf') mext
      pure $ archiveDir </> arf''

data NotInWorkflowDir = NotInWorkflowDir (Path Abs Dir) (Path Abs File)
  deriving (Show, Eq)

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
      let allDone = all (maybe True isDone . entryState) (concatMap flatten (smosFileForest sf))
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
        unlines
          [ unwords ["Not all entries in", fromAbsFile from, "are done."],
            "Are you sure that you want to archive it?",
            "All remaining non-done entries will be set to CANCELLED."
          ]
    case res of
      Yes -> pure sf
      No -> die "Not archiving."

isDone :: TodoState -> Bool
isDone "DONE" = True
isDone "CANCELLED" = True
isDone "FAILED" = True
isDone _ = False

data ArchiveMoveResult
  = MoveDestinationAlreadyExists (Path Abs File)
  | ArchivedSuccesfully
  deriving (Show, Eq)

moveToArchive :: MonadIO m => Path Abs File -> Path Abs File -> SmosFile -> m ArchiveMoveResult
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
setAllUndoneToCancelled now (SmosFile f) = SmosFile $ map (fmap go) f
  where
    go :: Entry -> Entry
    go e =
      case entryState e of
        Nothing -> e
        Just ts ->
          if isDone ts
            then e
            else fromMaybe e $ entrySetState now (Just "CANCELLED") e
