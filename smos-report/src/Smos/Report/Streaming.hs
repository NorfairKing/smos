{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Report.Streaming where

import Control.Exception
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Tree

import Lens.Micro

import Path
import Path.IO

import Conduit
import Cursor.Simple.Forest
import Cursor.Simple.Tree
import qualified Data.Conduit.Combinators as Conduit

import Smos.Data

import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Path
import Smos.Report.ShouldPrint

streamSmosFilesFromWorkflow ::
     MonadIO m => HideArchive -> SmosReportConfig -> ConduitT i RootedPath m ()
streamSmosFilesFromWorkflow ha src@SmosReportConfig {..} = do
  wd <- liftIO $ resolveReportWorkflowDir src
  case smosReportConfigArchiveFileSpec of
    ArchiveInWorkflow rf -> do
      let source =
            (case ha of
               HideArchive -> sourceFilesInNonHiddenDirsRecursivelyExceptSubdir rf wd
               Don'tHideArchive -> sourceFilesInNonHiddenDirsRecursively wd)
      source .| filterSmosFiles
    _ -> do
      ad <- liftIO $ resolveReportArchiveDir src
      let maybeFilterOutArchived =
            (case ha of
               HideArchive -> (filterOutDir ad .|)
               Don'tHideArchive -> id)
      sourceFilesInNonHiddenDirsRecursively wd .| maybeFilterOutArchived filterSmosFiles

-- TODO I think we can do fancier filtering based on the other ArchiveDirSpecs
filterOutDir :: Monad m => Path Abs Dir -> ConduitT RootedPath RootedPath m ()
filterOutDir ad = Conduit.filter (\rp -> not $ isProperPrefixOf ad $ resolveRootedPath rp)

sourceFilesInNonHiddenDirsRecursively ::
     forall m i. MonadIO m
  => Path Abs Dir
  -> ConduitT i RootedPath m ()
sourceFilesInNonHiddenDirsRecursively dir = walkSafe go dir
  where
    go :: Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> ConduitT i RootedPath m WalkAction
    go curdir subdirs files = do
      Conduit.yieldMany $ map (rootedIn dir) files
      pure $ WalkExclude $ filter (isHiddenIn curdir) subdirs

sourceFilesInNonHiddenDirsRecursivelyExceptSubdir ::
     forall m i. MonadIO m
  => Path Rel Dir
  -> Path Abs Dir
  -> ConduitT i RootedPath m ()
sourceFilesInNonHiddenDirsRecursivelyExceptSubdir subdir dir = walkSafe go dir
  where
    go :: Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> ConduitT i RootedPath m WalkAction
    go curdir subdirs files = do
      let addExtraFilter =
            if curdir == (dir </> subdir)
              then const subdirs
              else id
      Conduit.yieldMany $ map (rootedIn dir) files
      pure $ WalkExclude $ addExtraFilter $ filter (isHiddenIn curdir) subdirs

walkSafe ::
     MonadIO m
  => (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m WalkAction)
  -> Path Abs Dir
  -> m ()
walkSafe go dir = do
  e <- liftIO $ fmap (fromMaybe False) $ forgivingAbsence $ doesDirExist dir
  if e
    then walkDir go dir
    else pure ()

rootedIn :: Path Abs Dir -> Path Abs File -> RootedPath
rootedIn dir ap =
  case stripProperPrefix dir ap of
    Nothing -> Absolute ap
    Just rd -> Relative dir rd

isHiddenIn :: Path r Dir -> Path r Dir -> Bool
isHiddenIn curdir ad =
  case stripProperPrefix curdir ad of
    Nothing -> True
    Just rd -> "." `isPrefixOf` fromRelDir rd

filterSmosFiles :: Monad m => ConduitT RootedPath RootedPath m ()
filterSmosFiles =
  Conduit.filter $ \f ->
    case f of
      Relative _ prf -> fileExtension prf == ".smos"
      Absolute paf -> fileExtension paf == ".smos"

parseSmosFiles ::
     MonadIO m => ConduitT RootedPath (RootedPath, Either ParseSmosFileException SmosFile) m ()
parseSmosFiles =
  Conduit.mapM $ \p -> do
    let ap = resolveRootedPath p
    mErrOrSmosFile <- liftIO $ readSmosFile ap
    let ei =
          case mErrOrSmosFile of
            Nothing -> Left $ FileDoesntExist ap
            Just errOrSmosFile ->
              case errOrSmosFile of
                Left err -> Left $ SmosFileParseError ap err
                Right sf -> Right sf
    pure (p, ei)

printShouldPrint ::
     MonadIO m => ShouldPrint -> ConduitT (a, Either ParseSmosFileException b) (a, b) m ()
printShouldPrint sp =
  Conduit.concatMapM $ \(a, errOrB) ->
    case errOrB of
      Left err -> do
        printErrorMessage sp $ displayException err
        pure Nothing
      Right b -> pure $ Just (a, b)

data ParseSmosFileException
  = FileDoesntExist (Path Abs File)
  | SmosFileParseError (Path Abs File) String
  deriving (Show, Eq)

instance Exception ParseSmosFileException where
  displayException (FileDoesntExist file) = "The file " <> fromAbsFile file <> " does not exist."
  displayException (SmosFileParseError file errMess) =
    "The file " <> fromAbsFile file <> " cannot be parsed:\n\t" <> errMess

trimByTags :: Monad m => [Tag] -> ConduitT (a, SmosFile) (a, SmosFile) m ()
trimByTags ts = Conduit.map $ \(rf, SmosFile sfs) -> (rf, SmosFile $ goF sfs)
  where
    goF :: Forest Entry -> Forest Entry
    goF =
      concatMap $ \t ->
        case goT t of
          Left t_ -> [t_]
          Right fs -> fs
    goT :: Tree Entry -> Either (Tree Entry) (Forest Entry)
    goT t@(Node e fs) =
      if all (`elem` entryTags e) ts
        then Left t
        else Right $ goF fs

smosFileEntries :: Monad m => ConduitT (a, SmosFile) (a, Entry) m ()
smosFileEntries = Conduit.concatMap $ uncurry go
  where
    go :: a -> SmosFile -> [(a, Entry)]
    go rf = map ((,) rf) . concatMap flatten . smosFileForest

smosFileCursors :: Monad m => ConduitT (a, SmosFile) (a, ForestCursor Entry) m ()
smosFileCursors = Conduit.concatMap $ \(rf, sf) -> (,) rf <$> allCursors sf

smosCursorCurrents :: Monad m => ConduitT (a, ForestCursor Entry) (a, Entry) m ()
smosCursorCurrents = Conduit.map smosCursorCurrent

smosCursorCurrent :: (a, ForestCursor Entry) -> (a, Entry)
smosCursorCurrent = \(rf, fc) -> (rf, fc ^. forestCursorSelectedTreeL . treeCursorCurrentL)

allCursors :: SmosFile -> [ForestCursor Entry]
allCursors = concatMap flatten . forestCursors . smosFileForest

forestCursors :: Forest a -> Forest (ForestCursor a)
forestCursors ts =
  case NE.nonEmpty ts of
    Nothing -> []
    Just ne -> goTop (makeForestCursor $ NE.map (cTree True) ne)
  where
    goTop :: ForestCursor a -> Forest (ForestCursor a)
    goTop fc =
      go fc ++
      (case forestCursorSelectNextTreeCursor fc of
         Nothing -> []
         Just fc' -> goTop fc')
    go :: ForestCursor a -> Forest (ForestCursor a)
    go fc =
      Node
        fc
        (case forestCursorSelectBelowAtStart fc of
           Nothing -> []
           Just fc' -> go fc') :
      (case (fc & forestCursorSelectedTreeL treeCursorSelectNextOnSameLevel) of
         Nothing -> []
         Just fc' -> go fc')
