{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Clock where

import GHC.Generics (Generic)

import Data.List
import Data.Maybe

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Time
import Data.Tree

import Text.Show.Pretty

import Path
import Path.IO

import Conduit
import qualified Data.Conduit.Combinators as C

import Smos.Data

import Smos.Report.Formatting
import Smos.Report.OptParse
import Smos.Report.Parse

clock :: Settings -> IO ()
clock Settings {..} = do
    tups <-
        sourceToList $
        sourceNonhiddenFiles setWorkDir .| filterSmosFiles .|
        parseSmosFiles setWorkDir setShouldPrint
    T.putStrLn $ renderClockTable $ makeClockTable tups

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
                        printErrorMessages sp $
                        displayErrMess [FileDoesntExist ap]
                    Just errOrSmosFile ->
                        case errOrSmosFile of
                            Left err ->
                                lift $
                                printErrorMessages sp $
                                displayErrMess [SmosFileParseError ap err]
                            Right sf -> yield (p, sf)
                loop

data ClockTableEntry = ClockTableEntry
    { clockTableEntryFile :: Path Rel File
    , clockTableEntryHeader :: Header
    , clockTableEntryTime :: NominalDiffTime
    } deriving (Show, Eq, Generic)

makeClockTable :: [(Path Rel File, SmosFile)] -> [ClockTableEntry]
makeClockTable = concatMap $ uncurry go
  where
    go :: Path Rel File -> SmosFile -> [ClockTableEntry]
    go rf = mapMaybe go' . concatMap flatten . smosFileForest
      where
        go' :: Entry -> Maybe ClockTableEntry
        go' Entry {..} =
            let t = sumLogbookTime entryLogbook
             in if t > 0
                    then Just
                             ClockTableEntry
                                 { clockTableEntryFile = rf
                                 , clockTableEntryHeader = entryHeader
                                 , clockTableEntryTime = t
                                 }
                    else Nothing

sumLogbookTime :: Logbook -> NominalDiffTime
sumLogbookTime lb =
    sum $
    case lb of
        (LogOpen _ es) -> map go es
        (LogClosed es) -> map go es
  where
    go :: LogbookEntry -> NominalDiffTime
    go LogbookEntry {..} = diffUTCTime logbookEntryEnd logbookEntryStart

renderClockTable :: [ClockTableEntry] -> Text
renderClockTable = T.pack . formatAsTable . map go
  where
    go :: ClockTableEntry -> [String]
    go ClockTableEntry {..} =
        [ fromRelFile clockTableEntryFile
        , T.unpack $ headerText clockTableEntryHeader
        , show clockTableEntryTime
        ]
