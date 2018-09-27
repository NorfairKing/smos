{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Wait
    ( wait
    ) where

import Import

import qualified Data.Text as T
import Data.Time.Clock

import Smos.Data
import Smos.Report.Formatting
import Smos.Report.OptParse
import Smos.Report.Parse

wait :: Settings -> IO ()
wait set@Settings {..} = do
    waitingTaskInfos <-
        applyToAllSmosFiles set $ \file smosfile ->
            pure . Right $ getWaitingTasks file smosfile
    now <- getCurrentTime
    putStr . formatAsTable $
        formatWaitingTaskInfo now <$> sort (concat waitingTaskInfos)

getWaitingTasks :: Path Abs File -> SmosFile -> [WaitingTaskInfo]
getWaitingTasks file smosfile =
    waitingTasks <&> \Entry {..} ->
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
isWaitingTask entry = entryState entry == Just (TodoState "WAITING")

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
    toString t = show (diffDays now t) <> " days"
    diffDays t1 t2 = floor $ diffUTCTime t1 t2 / nominalDay
