{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.Commands.Next
  ( next,
  )
where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Smos.Query.Formatting
import Smos.Report.Time
import Smos.Scheduler.OptParse
import Smos.Scheduler.Recurrence
import Smos.Scheduler.Utils
import System.Cron (serializeCronSchedule)
import Text.Colour.Term
import Text.Time.Pretty

next :: Settings -> IO ()
next Settings {..} = do
  mState <- readStateFile setStateFile
  now <- getZonedTime
  nextRows <- forM (scheduleItems setSchedule) $ \si -> do
    let mLastRun = computeLastRun mState si
    mNextRun <- computeNextRun setDirectorySettings mState (zonedTimeToUTC now) si
    pure
      NextRow
        { nextRowDescription = scheduleItemDescription si,
          nextRowRecurrence = scheduleItemRecurrence si,
          nextRowLastRun = mLastRun,
          nextRowNextRun = mNextRun
        }

  let headerRow = map (underline . fore white) ["Schedule item", "Recurrence", "Last activation", "Next activation"]
  putChunks $
    formatAsBicolourTable setColourSettings $
      headerRow : map (renderNextRow now) nextRows

data NextRow = NextRow
  { nextRowDescription :: !(Maybe Text),
    nextRowRecurrence :: !Recurrence,
    nextRowLastRun :: !(Maybe UTCTime),
    nextRowNextRun :: !(Maybe UTCTime)
  }
  deriving (Show, Eq)

renderNextRow :: ZonedTime -> NextRow -> [Chunk]
renderNextRow now NextRow {..} =
  let tz = zonedTimeZone now
   in [ fore blue . chunk $ fromMaybe "" nextRowDescription,
        fore magenta . chunk $
          case nextRowRecurrence of
            HaircutRecurrence t -> renderTime t
            RentRecurrence cs -> serializeCronSchedule cs,
        fore yellow . chunk . T.pack $ case nextRowLastRun of
          Nothing -> ""
          Just lastRun ->
            unwords
              [ formatTime defaultTimeLocale "%F %H:%M" (utcToLocalTime tz lastRun),
                "-",
                prettyTimeAuto (zonedTimeToUTC now) lastRun
              ],
        fore yellow . chunk . T.pack $ case nextRowNextRun of
          Nothing -> case nextRowRecurrence of
            RentRecurrence _ -> "never again"
            HaircutRecurrence _ -> "still in progress"
          Just nextRun ->
            unwords
              [ formatTime defaultTimeLocale "%F %H:%M" (utcToLocalTime tz nextRun),
                "-",
                prettyTimeAuto (zonedTimeToUTC now) nextRun
              ]
      ]
