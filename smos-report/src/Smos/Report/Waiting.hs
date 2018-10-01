{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Waiting
    ( waiting
    ) where

import Data.Function
import Data.List
import qualified Data.Text as T
import Data.Time.Clock

import Path

import Smos.Data
import Smos.Report.Formatting
import Smos.Report.OptParse
import Smos.Report.Parse

waiting :: Settings -> IO ()
waiting set@Settings {..} = do
    waitingTaskInfos <-
        applyToAllSmosFiles set $ \file smosfile ->
            pure . Right $ getWaitingTasks file smosfile
    now <- getCurrentTime
    putStr . formatAsTable $
        formatWaitingTaskInfo now <$> sort (concat waitingTaskInfos)

getWaitingTasks :: Path Abs File -> SmosFile -> [WaitingTaskInfo]
getWaitingTasks file smosfile =
    flip map waitingTasks $ \Entry {..} ->
        let time =
                case unStateHistory entryStateHistory of
                    [] -> Nothing
                    x:_ -> Just $ stateHistoryEntryTimestamp x
         in WaitingTaskInfo
                { wtFile = filename file
                , wtHeader = entryHeader
                , wtTimestamp = time
                }
  where
    waitingTasks = filter isWaitingTask $ entries smosfile :: [Entry]

isWaitingTask :: Entry -> Bool
isWaitingTask entry = entryState entry == Just "WAITING"

data WaitingTaskInfo = WaitingTaskInfo
    { wtFile :: Path Rel File
    , wtHeader :: Header
    , wtTimestamp :: Maybe UTCTime
    } deriving (Show, Eq)

instance Ord WaitingTaskInfo where
    (<=) = (<=) `on` wtTimestamp

formatWaitingTaskInfo :: UTCTime -> WaitingTaskInfo -> [String]
formatWaitingTaskInfo currentTime WaitingTaskInfo {..} =
    [ fromRelFile wtFile
    , T.unpack $ headerText wtHeader
    , showDays currentTime wtTimestamp
    ]

showDays :: UTCTime -> Maybe UTCTime -> String
showDays now = maybe "" toString
  where
    toString t = show (diffDays now t :: Int) <> " days"
    diffDays t1 t2 = floor $ diffUTCTime t1 t2 / nominalDay
