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
import Data.Time.Zones
import Smos.CLI.Formatting
import Smos.Report.Time
import Smos.Scheduler.OptParse
import Smos.Scheduler.Recurrence
import System.Cron (serializeCronSchedule)
import Text.Colour
import Text.Colour.Term
import Text.Time.Pretty

next :: Settings -> IO ()
next Settings {..} = do
  zone <- loadLocalTZ
  now <- getCurrentTime
  rh <- readReccurrenceHistory setDirectorySettings
  nextRows <- forM (scheduleItems setSchedule) $ \si -> do
    let mLastRun = computeLastRun rh (hashScheduleItem si)
    let mNextRun = computeNextRun zone now rh si
    pure
      NextRow
        { nextRowDescription = scheduleItemDescription si,
          nextRowRecurrence = scheduleItemRecurrence si,
          nextRowLastRun = mLastRun,
          nextRowNextRun = mNextRun
        }

  let headerRow = map (underline . fore white) ["Schedule item", "Recurrence", "Last activation", "Next activation"]
  putChunksLocale $
    formatAsBicolourTable setColourSettings $
      headerRow : map (renderNextRow zone now) nextRows

data NextRow = NextRow
  { nextRowDescription :: !(Maybe Text),
    nextRowRecurrence :: !Recurrence,
    nextRowLastRun :: !(Maybe UTCTime),
    nextRowNextRun :: !(Either HaircutNextRun RentNextRun)
  }

renderNextRow :: TZ -> UTCTime -> NextRow -> [Chunk]
renderNextRow zone now NextRow {..} =
  let nowLocal = utcToLocalTimeTZ zone now
      prettyRelative = prettyTimeAuto now
   in [ fore blue . chunk $ fromMaybe "" nextRowDescription,
        fore magenta . chunk $
          case nextRowRecurrence of
            HaircutRecurrence t -> renderTime t
            RentRecurrence cs -> serializeCronSchedule cs,
        fore yellow . chunk . T.pack $ case nextRowLastRun of
          Nothing -> ""
          Just lastRun ->
            unwords
              [ formatTime defaultTimeLocale "%F %H:%M" (utcToLocalTimeTZ zone lastRun),
                "-",
                prettyRelative lastRun
              ],
        fore yellow . chunk . T.pack $ case nextRowNextRun of
          Left hnr -> case hnr of
            DoNotActivateHaircut -> "still in progress"
            ActivateHaircutImmediately ->
              unwords
                [ formatTime defaultTimeLocale "%F %H:%M" nowLocal,
                  "-",
                  prettyRelative now
                ]
            ActivateHaircutNoSoonerThan nextRun ->
              unwords
                [ formatTime defaultTimeLocale "%F %H:%M" (utcToLocalTimeTZ zone nextRun),
                  "-",
                  prettyRelative nextRun
                ]
          Right rnr -> case rnr of
            DoNotActivateRent -> "never (again)"
            ActivateRentImmediatelyAsIfAt nextRun ->
              unwords
                [ formatTime defaultTimeLocale "%F %H:%M" nextRun,
                  "-",
                  prettyRelative (localTimeToUTCTZ zone nextRun)
                ]
            ActivateRentNoSoonerThan nextRun ->
              unwords
                [ formatTime defaultTimeLocale "%F %H:%M" nextRun,
                  "-",
                  prettyRelative (localTimeToUTCTZ zone nextRun)
                ]
      ]
