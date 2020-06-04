{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Report.Streaming where

import Conduit
import Control.Exception
import Cursor.Simple.Forest
import Cursor.Simple.Tree
import qualified Data.Conduit.Combinators as Conduit
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Tree
import Lens.Micro
import Path
import Path.IO
import Smos.Data
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.Path
import Smos.Report.ShouldPrint

streamSmosProjectsFiles :: MonadIO m => DirectoryConfig -> ConduitT i RootedPath m ()
streamSmosProjectsFiles dc = do
  pd <- liftIO $ resolveDirProjectsDir dc
  sourceFilesInNonHiddenDirsRecursively pd .| filterSmosFiles

-- TODO eventually get rid of this
streamSmosFilesFromWorkflow ::
  MonadIO m => HideArchive -> DirectoryConfig -> ConduitT i RootedPath m ()
streamSmosFilesFromWorkflow ha dc = do
  wd <- liftIO $ resolveDirWorkflowDir dc
  case directoryConfigArchiveFileSpec dc of
    ArchiveInWorkflow rf -> do
      let source =
            case ha of
              HideArchive -> sourceFilesInNonHiddenDirsRecursivelyExceptSubdir rf wd
              Don'tHideArchive -> sourceFilesInNonHiddenDirsRecursively wd
      source .| filterSmosFiles
    _ -> do
      ad <- liftIO $ resolveDirArchiveDir dc
      let maybeFilterOutArchived =
            case ha of
              HideArchive -> (filterOutDir ad .|)
              Don'tHideArchive -> id
      sourceFilesInNonHiddenDirsRecursively wd .| maybeFilterOutArchived filterSmosFiles

-- TODO I think we can do fancier filtering based on the other ArchiveDirSpecs
filterOutDir :: Monad m => Path Abs Dir -> ConduitT RootedPath RootedPath m ()
filterOutDir ad = Conduit.filter (not . isProperPrefixOf ad . resolveRootedPath)

streamSmosFilesFromWorkflowRel ::
  MonadIO m => HideArchive -> DirectoryConfig -> ConduitT i (Path Rel File) m ()
streamSmosFilesFromWorkflowRel ha dc = do
  wd <- liftIO $ resolveDirWorkflowDir dc
  case directoryConfigArchiveFileSpec dc of
    ArchiveInWorkflow rf -> do
      let source =
            case ha of
              HideArchive -> sourceFilesInNonHiddenDirsRecursivelyExceptSubdirRel rf wd
              Don'tHideArchive -> sourceFilesInNonHiddenDirsRecursivelyRel wd
      source .| filterSmosFilesRel
    _ -> do
      ad <- liftIO $ resolveDirArchiveDir dc
      let maybeFilterOutArchived =
            case ha of
              HideArchive -> (Conduit.filter (not . isProperPrefixOf ad . (wd </>)) .|)
              Don'tHideArchive -> id
      sourceFilesInNonHiddenDirsRecursivelyRel wd .| maybeFilterOutArchived filterSmosFilesRel

-- TODO eventually remove this one.
sourceFilesInNonHiddenDirsRecursively ::
  forall m i.
  MonadIO m =>
  Path Abs Dir ->
  ConduitT i RootedPath m ()
sourceFilesInNonHiddenDirsRecursively dir = sourceFilesInNonHiddenDirsRecursivelyRel dir .| Conduit.map (Relative dir)

sourceFilesInNonHiddenDirsRecursivelyRel ::
  forall m i.
  MonadIO m =>
  Path Abs Dir ->
  ConduitT i (Path Rel File) m ()
sourceFilesInNonHiddenDirsRecursivelyRel = walkSafeRel go
  where
    go ::
      Path Rel Dir ->
      [Path Rel Dir] ->
      [Path Rel File] ->
      ConduitT i (Path Rel File) m (WalkAction Rel)
    go curdir subdirs files = do
      Conduit.yieldMany $ map (curdir </>) files
      pure $ WalkExclude $ filter (isHiddenIn curdir) subdirs

-- TODO eventually remove this one
sourceFilesInNonHiddenDirsRecursivelyExceptSubdir ::
  forall m i.
  MonadIO m =>
  Path Rel Dir ->
  Path Abs Dir ->
  ConduitT i RootedPath m ()
sourceFilesInNonHiddenDirsRecursivelyExceptSubdir subdir dir = sourceFilesInNonHiddenDirsRecursivelyExceptSubdirRel subdir dir .| Conduit.map (Relative dir)

sourceFilesInNonHiddenDirsRecursivelyExceptSubdirRel ::
  forall m i.
  MonadIO m =>
  Path Rel Dir ->
  Path Abs Dir ->
  ConduitT i (Path Rel File) m ()
sourceFilesInNonHiddenDirsRecursivelyExceptSubdirRel subdir = walkSafeRel go
  where
    go ::
      Path Rel Dir ->
      [Path Rel Dir] ->
      [Path Rel File] ->
      ConduitT i (Path Rel File) m (WalkAction Rel)
    go curdir subdirs files = do
      let addExtraFilter =
            if curdir == subdir
              then const subdirs
              else id
      Conduit.yieldMany $ map (curdir </>) files
      pure $ WalkExclude $ addExtraFilter $ filter (isHiddenIn curdir) subdirs

walkSafeRel ::
  MonadIO m =>
  (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m (WalkAction Rel)) ->
  Path Abs Dir ->
  m ()
walkSafeRel go dir = do
  e <- liftIO $ fmap (fromMaybe False) $ forgivingAbsence $ doesDirExist dir
  if e
    then walkDirRel go dir
    else pure ()

rootedIn :: Path Abs Dir -> Path Abs File -> RootedPath
rootedIn dir ap =
  case stripProperPrefix dir ap of
    Nothing -> Absolute ap
    Just rd -> Relative dir rd

isHiddenIn :: Path b Dir -> Path b t -> Bool
isHiddenIn curdir ad =
  case stripProperPrefix curdir ad of
    Nothing -> False
    Just rd -> "." `isPrefixOf` toFilePath rd

filterSmosFiles :: Monad m => ConduitT RootedPath RootedPath m ()
filterSmosFiles =
  Conduit.filter $ \case
    Relative _ prf -> fileExtension prf == ".smos"
    Absolute paf -> fileExtension paf == ".smos"

filterSmosFilesRel :: Monad m => ConduitT (Path b File) (Path b File) m ()
filterSmosFilesRel =
  Conduit.filter $ (== ".smos") . fileExtension

-- TODO eventually get rid of this
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

parseSmosFilesRel ::
  MonadIO m => Path Abs Dir -> ConduitT (Path Rel File) (Path Rel File, Either ParseSmosFileException SmosFile) m ()
parseSmosFilesRel dir =
  Conduit.mapM $ \rf -> do
    let ap = dir </> rf
    mErrOrSmosFile <- liftIO $ readSmosFile ap
    let ei =
          case mErrOrSmosFile of
            Nothing -> Left $ FileDoesntExist ap
            Just errOrSmosFile ->
              case errOrSmosFile of
                Left err -> Left $ SmosFileParseError ap err
                Right sf -> Right sf
    pure (rf, ei)

smosFilter :: Monad m => Filter a -> ConduitT a a m ()
smosFilter f = Conduit.filter (filterPredicate f)

smosMFilter :: Monad m => Maybe (Filter a) -> ConduitT a a m ()
smosMFilter = maybe (Conduit.map id) smosFilter

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
smosCursorCurrent (rf, fc) = (rf, forestCursorCurrent fc)

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
      go fc
        ++ ( case forestCursorSelectNextTreeCursor fc of
               Nothing -> []
               Just fc' -> goTop fc'
           )
    go :: ForestCursor a -> Forest (ForestCursor a)
    go fc =
      Node
        fc
        ( case forestCursorSelectBelowAtStart fc of
            Nothing -> []
            Just fc' -> go fc'
        )
        : ( case fc & forestCursorSelectedTreeL treeCursorSelectNextOnSameLevel of
              Nothing -> []
              Just fc' -> go fc'
          )

accumulateSink :: Monad m => (a -> a -> a) -> a -> ConduitT a void m a
accumulateSink operation = go
  where
    go !a = do
      mn <- await
      case mn of
        Nothing -> pure a
        Just n -> go $ a `operation` n

accumulateMonoid :: (Monoid a, Monad m) => ConduitT a void m a
accumulateMonoid = accumulateSink mappend mempty
