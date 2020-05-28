{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Archive
  ( smosArchive,

    -- ** Helper functions
    moveToArchive,
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
import Smos.Archive.OptParse.Types
import Smos.Archive.Prompt
import Smos.Data
import Smos.Report.Config
import System.Exit

smosArchive :: IO ()
smosArchive = do
  Settings {..} <- getSettings
  runReaderT (archive setFile) setReportSettings

type Q a = ReaderT SmosReportConfig IO a

archive :: Path Abs File -> Q ()
archive from = do
  to <- determineToFile from
  liftIO $ do
    checkFromFile from >>= dealWithArchiveCheckResult from
    moveToArchive from to >>= dealWithArchiveMoveResult from

determineToFile :: Path Abs File -> Q (Path Abs File)
determineToFile file = do
  workflowDir <- asks resolveReportWorkflowDir >>= liftIO
  archiveDir <- asks resolveReportArchiveDir >>= liftIO
  today <- liftIO $ utctDay <$> getCurrentTime
  destinationFile today workflowDir archiveDir file

destinationFile ::
  MonadThrow m =>
  Day ->
  -- | Workflow dir
  Path Abs Dir ->
  -- | Archive dir
  Path Abs Dir ->
  -- | File to archive
  Path Abs File ->
  -- | Archive file
  m (Path Abs File)
destinationFile today workflowDir archiveDir file = do
  case stripProperPrefix workflowDir file of
    Nothing -> throwM (NotInWorkflowDir workflowDir file)
    Just rf -> do
      let ext = fileExtension rf
      withoutExt <- setFileExtension "" rf
      let newRelFile = fromRelFile withoutExt ++ "_" ++ formatTime defaultTimeLocale "%F" today
      arf' <- parseRelFile newRelFile
      arf'' <- setFileExtension ext arf'
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
  | NotAllDone
  | ReadyToArchive

checkFromFile :: Path Abs File -> IO ArchiveCheckResult
checkFromFile from = do
  mErrOrSF <- readSmosFile from
  pure $ case mErrOrSF of
    Nothing -> CheckFileToArchiveDoesNotExist
    Just (Left err) -> CheckNotASmosFile err
    Just (Right sf) ->
      let allDone = all (maybe True isDone . entryState) (concatMap flatten (smosFileForest sf))
       in if allDone
            then ReadyToArchive
            else NotAllDone

dealWithArchiveCheckResult :: Path Abs File -> ArchiveCheckResult -> IO ()
dealWithArchiveCheckResult from = \case
  ReadyToArchive -> pure ()
  CheckFileToArchiveDoesNotExist -> die $ unwords ["File does not exist:", fromAbsFile from]
  CheckNotASmosFile err -> die $ unlines [unwords ["The file to archive doesn't look like a smos file:", fromAbsFile from], err]
  NotAllDone -> do
    res <-
      promptYesNo No $
        unlines
          [ unwords ["Not all entries in", fromAbsFile from, "are done."],
            "Are you sure that you want to archive it?",
            "All remaining non-done entries will be set to CANCELLED."
          ]
    case res of
      Yes -> pure ()
      No -> die "Not archiving."

isDone :: TodoState -> Bool
isDone "DONE" = True
isDone "CANCELLED" = True
isDone "FAILED" = True
isDone _ = False

data ArchiveMoveResult
  = MoveFileToArchiveDoesNotExist
  | MoveNotASmosFile String
  | MoveDestinationAlreadyExists (Path Abs File)
  | ArchivedSuccesfully
  deriving (Show, Eq)

moveToArchive :: Path Abs File -> Path Abs File -> IO ArchiveMoveResult
moveToArchive from to = do
  ensureDir $ parent to
  mErrOrSmosFile <- readSmosFile from
  case mErrOrSmosFile of
    Nothing -> pure MoveFileToArchiveDoesNotExist
    Just (Left err) -> pure $ MoveNotASmosFile err
    Just (Right sf) -> do
      e2 <- doesFileExist to
      if e2
        then pure $ MoveDestinationAlreadyExists to
        else do
          now <- liftIO getCurrentTime
          let archivedSmosFile = prepareToArchive now sf
          writeSmosFile to archivedSmosFile
          removeFile from
          pure ArchivedSuccesfully

dealWithArchiveMoveResult :: Path Abs File -> ArchiveMoveResult -> IO ()
dealWithArchiveMoveResult from = \case
  ArchivedSuccesfully -> pure ()
  MoveFileToArchiveDoesNotExist -> die $ unwords ["File does not exist:", fromAbsFile from]
  MoveNotASmosFile err -> die $ unlines [unwords ["The file to archive doesn't look like a smos file:", fromAbsFile from], err]
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
