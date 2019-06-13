{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Waiting
  ( waiting
  ) where

import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Report.Query
import Smos.Report.Streaming
import Smos.Report.Waiting

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types

waiting :: WaitingSettings -> Q ()
waiting WaitingSettings {..} = do
  wd <- askWorkDir
  liftIO $ do
    tups <-
      sourceToList $
      sourceFilesInNonHiddenDirsRecursively wd .| filterSmosFiles .|
      parseSmosFiles .|
      printShouldPrint PrintWarning .|
      smosFileCursors .|
      C.filter
        (\(rp, fc) ->
           maybe True (\f -> filterPredicate f rp fc) waitingSetFilter) .|
      smosCursorCurrents .|
      C.filter (isWaitingAction . snd) .|
      C.map (uncurry makeWaitingActionEntry)
    now <- getCurrentTime
    putTableLn $ renderWaitingActionReport now tups

renderWaitingActionReport :: UTCTime -> [WaitingActionEntry] -> Table
renderWaitingActionReport now =
  formatAsTable .
  map (formatWaitingActionEntry now) . sortOn waitingActionEntryTimestamp

formatWaitingActionEntry :: UTCTime -> WaitingActionEntry -> [Chunk Text]
formatWaitingActionEntry now WaitingActionEntry {..} =
  [ rootedPathChunk waitingActionEntryFilePath
  , headerChunk $ waitingActionEntryHeader
  , maybe (chunk "") (showDaysSince now) waitingActionEntryTimestamp
  ]

showDaysSince :: UTCTime -> UTCTime -> Chunk Text
showDaysSince now t = fore color $ chunk $ T.pack $ show i <> " days"
  where
    color
      | i > 21 = red
      | i > 15 = yellow
      | i > 5 = blue
      | otherwise = mempty
    i = diffInDays now t :: Int
    diffInDays :: UTCTime -> UTCTime -> Int
    diffInDays t1 t2 = floor $ diffUTCTime t1 t2 / nominalDay
