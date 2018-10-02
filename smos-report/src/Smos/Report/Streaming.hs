{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Streaming where

import Control.Exception

import Data.List
import Data.Maybe

import Data.Tree

import Path
import Path.IO

import Conduit
import qualified Data.Conduit.Combinators as C

import Smos.Data

import Smos.Report.OptParse

sourceFilesInNonHiddenDirsRecursively ::
       Path Abs Dir -> ConduitT i (Path Rel File) IO ()
sourceFilesInNonHiddenDirsRecursively dir = walkDir go dir
  where
    go :: Path Abs Dir
       -> [Path Abs Dir]
       -> [Path Abs File]
       -> ConduitT i (Path Rel File) IO WalkAction
    go curdir subdirs files = do
        C.yieldMany $ mapMaybe (stripProperPrefix dir) files
        pure $ WalkExclude $ filter hidden subdirs
      where
        hidden ad =
            case stripProperPrefix curdir ad of
                Nothing -> True
                Just rd -> ("." `isPrefixOf` fromRelDir rd)

filterSmosFiles :: Monad m => ConduitT (Path r File) (Path r File) m ()
filterSmosFiles = C.filter $ (== ".smos") . fileExtension

parseSmosFiles ::
       Path Abs Dir
    -> ConduitT (Path Rel File) ( Path Rel File
                                , Either ParseSmosFileException SmosFile) IO ()
parseSmosFiles dir =
    C.mapM $ \p -> do
        let ap = dir </> p
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
    | SmosFileParseError (Path Abs File)
                         String
    deriving (Show, Eq)

instance Exception ParseSmosFileException where
    displayException (FileDoesntExist file) =
        "The file " <> fromAbsFile file <> " does not exist."
    displayException (SmosFileParseError file errMess) =
        "The file " <> fromAbsFile file <> " cannot be parsed:\n\t" <> errMess

smosFileEntries ::
       Monad m => ConduitT (Path Rel File, SmosFile) (Path Rel File, Entry) m ()
smosFileEntries = C.concatMap $ uncurry go
  where
    go :: Path Rel File -> SmosFile -> [(Path Rel File, Entry)]
    go rf = map ((,) rf) . concatMap flatten . smosFileForest
