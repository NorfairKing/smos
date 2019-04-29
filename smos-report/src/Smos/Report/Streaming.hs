{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

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
import qualified Data.Conduit.Combinators as C

import Smos.Data

import Smos.Report.Path
import Smos.Report.ShouldPrint

sourceFilesInNonHiddenDirsRecursively ::
       Path Abs Dir -> ConduitT i RootedPath IO ()
sourceFilesInNonHiddenDirsRecursively dir = walkDir go dir
  where
    go :: Path Abs Dir
       -> [Path Abs Dir]
       -> [Path Abs File]
       -> ConduitT i RootedPath IO WalkAction
    go curdir subdirs files = do
        C.yieldMany $ map (Relative dir) $
            mapMaybe (stripProperPrefix dir) files
        pure $ WalkExclude $ filter hidden subdirs
      where
        hidden ad =
            case stripProperPrefix curdir ad of
                Nothing -> True
                Just rd -> ("." `isPrefixOf` fromRelDir rd)

filterSmosFiles :: Monad m => ConduitT RootedPath RootedPath m ()
filterSmosFiles =
    C.filter $ \f ->
        case f of
            Relative _ prf -> fileExtension prf == ".smos"
            Absolute paf -> fileExtension paf == ".smos"

parseSmosFiles ::
       ConduitT RootedPath (RootedPath, Either ParseSmosFileException SmosFile) IO ()
parseSmosFiles =
    C.mapM $ \p -> do
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
       ShouldPrint -> ConduitT (a, Either ParseSmosFileException b) (a, b) IO ()
printShouldPrint sp =
    C.concatMapM $ \(a, errOrB) ->
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
    displayException (FileDoesntExist file) =
        "The file " <> fromAbsFile file <> " does not exist."
    displayException (SmosFileParseError file errMess) =
        "The file " <> fromAbsFile file <> " cannot be parsed:\n\t" <> errMess

trimByTags :: Monad m => [Tag] -> ConduitT (a, SmosFile) (a, SmosFile) m ()
trimByTags ts = C.map $ \(rf, SmosFile sfs) -> (rf, SmosFile $ goF sfs)
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
smosFileEntries = C.concatMap $ uncurry go
  where
    go :: a -> SmosFile -> [(a, Entry)]
    go rf = map ((,) rf) . concatMap flatten . smosFileForest

smosFileCursors ::
       Monad m => ConduitT (a, SmosFile) (a, ForestCursor Entry) m ()
smosFileCursors = C.concatMap $ \(rf, sf) -> (,) rf <$> allCursors sf

smosCursorCurrents ::
       Monad m => ConduitT (a, ForestCursor Entry) (a, Entry) m ()
smosCursorCurrents = C.map smosCursorCurrent

smosCursorCurrent :: (a, ForestCursor Entry) -> (a, Entry)
smosCursorCurrent =
    \(rf, fc) -> (rf, fc ^. forestCursorSelectedTreeL . treeCursorCurrentL)

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
