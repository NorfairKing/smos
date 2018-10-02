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

sourceNonhiddenFiles :: Path Abs Dir -> ConduitT i (Path Rel File) IO ()
sourceNonhiddenFiles dir = walkDir go dir
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
    -> ShouldPrint
    -> ConduitT (Path Rel File) (Path Rel File, SmosFile) IO ()
parseSmosFiles dir sp = loop
  where
    loop = do
        mp <- await
        case mp of
            Nothing -> pure ()
            Just p -> do
                let ap = dir </> p
                mErrOrSmosFile <- lift $ readSmosFile ap
                case mErrOrSmosFile of
                    Nothing ->
                        lift $
                        printErrorMessage sp $
                        displayException $ FileDoesntExist ap
                    Just errOrSmosFile ->
                        case errOrSmosFile of
                            Left err ->
                                lift $
                                printErrorMessage sp $
                                displayException $ SmosFileParseError ap err
                            Right sf -> yield (p, sf)
                loop

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
