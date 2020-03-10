{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Waiting where

import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Report.Streaming
import Smos.Report.Waiting

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming

waiting :: WaitingSettings -> Q ()
waiting WaitingSettings {..} = do
  tups <-
    sourceToList $
    streamSmosFiles waitingSetHideArchive .| parseSmosFiles .| printShouldPrint PrintWarning .|
    smosFileCursors .|
    smosMFilter waitingSetFilter .|
    smosCursorCurrents .|
    C.filter (isWaitingAction . snd) .|
    C.map (uncurry makeWaitingActionEntry)
  now <- liftIO getCurrentTime
  liftIO $ putTableLn $ renderWaitingActionReport waitingSetThreshold now tups

renderWaitingActionReport :: Word -> UTCTime -> [WaitingActionEntry] -> Table
renderWaitingActionReport threshold now =
  formatAsTable . map (formatWaitingActionEntry threshold now) . sortOn waitingActionEntryTimestamp

formatWaitingActionEntry :: Word -> UTCTime -> WaitingActionEntry -> [Chunk Text]
formatWaitingActionEntry threshold now WaitingActionEntry {..} =
  [ rootedPathChunk waitingActionEntryFilePath
  , headerChunk waitingActionEntryHeader
  , maybe (chunk "") (showDaysSince threshold now) waitingActionEntryTimestamp
  ]

showDaysSince :: Word -> UTCTime -> UTCTime -> Chunk Text
showDaysSince threshold now t = fore color $ chunk $ T.pack $ show i <> " days"
  where
    th1 = fromIntegral threshold :: Int
    th2 = floor ((fromIntegral threshold :: Double) / 3 * 2) :: Int
    th3 = floor ((fromIntegral threshold :: Double) / 3) :: Int
    color
      | i >= th1 = red
      | i >= th2 = yellow
      | i >= th3 = blue
      | otherwise = green
    i = diffInDays now t :: Int
    diffInDays :: UTCTime -> UTCTime -> Int
    diffInDays t1 t2 = floor $ diffUTCTime t1 t2 / nominalDay
