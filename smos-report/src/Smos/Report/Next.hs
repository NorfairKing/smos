{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Next where

import Data.Maybe
import qualified Data.Text as T
import Data.Tree

import Path

import Smos.Data
import Smos.Report.Formatting
import Smos.Report.OptParse
import Smos.Report.Parse

next :: Settings -> IO ()
next set@Settings {..} = do
    nextTaskInfos <- applyToAllSmosFiles set getNextTasks
    putStr . formatAsTable $ formatNextTaskInfo <$> concat nextTaskInfos

formatNextTaskInfo :: NextTaskInfo -> [String]
formatNextTaskInfo NextTaskInfo {..} =
    [T.unpack $ headerText ntHeader, fromRelFile ntFile]

data NextTaskInfo = NextTaskInfo
    { ntHeader :: Header
    , ntFile :: Path Rel File
    } deriving (Show, Eq)

getNextTasks ::
       Path Abs File
    -> SmosFile
    -> IO (Either ProcessSmosFileException [NextTaskInfo])
getNextTasks file SmosFile {..} =
    case filter isNextAction $ concat $ flatten <$> smosFileForest of
        [] -> pure . Left $ NoNextActions file
        xs -> pure . Right . catMaybes $ getNextTask <$> xs
  where
    getNextTask :: Entry -> Maybe NextTaskInfo
    getNextTask entry@Entry {..} =
        if entryState entry == Just (TodoState "NEXT")
            then Just
                     NextTaskInfo
                         {ntHeader = entryHeader, ntFile = filename file}
            else Nothing

isNextAction :: Entry -> Bool
isNextAction entry =
    or $
    (==) (entryState entry) . Just . TodoState <$>
    ["WAITING", "NEXT", "STARTED", "READY"]
