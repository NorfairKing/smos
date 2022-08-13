{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.Commands.Next
  ( next,
  )
where

import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Smos.Query.Formatting
import Smos.Scheduler.OptParse
import Smos.Scheduler.Recurrence
import Smos.Scheduler.Utils
import System.Cron (nextMatch)
import Text.Colour.Term
import Text.Time.Pretty

next :: Settings -> IO ()
next Settings {..} = do
  mState <- readStateFile setStateFile
  now <- getZonedTime
  nextRows <- forM (scheduleItems setSchedule) $ \si -> do
    let mLastRun = computeLastRun mState si
    mNextRun <- computeNextRun (zonedTimeToUTC now) mState si
    pure
      NextRow
        { nextRowDescription = scheduleItemDescription si,
          nextRowLastRun = mLastRun,
          nextRowNextRun = mNextRun
        }

  let headerRow = map underline ["Schedule item", "Last activation", "", "Next activation", ""]
  putChunks $
    formatAsBicolourTable setColourSettings $
      headerRow : map (renderNextRow now) nextRows

data NextRow = NextRow
  { nextRowDescription :: Maybe Text,
    nextRowLastRun :: Maybe UTCTime,
    nextRowNextRun :: Maybe UTCTime
  }
  deriving (Show, Eq)

renderNextRow :: ZonedTime -> NextRow -> [Chunk]
renderNextRow now NextRow {..} =
  let tz = zonedTimeZone now
   in [ chunk $ fromMaybe "" nextRowDescription,
        maybe (chunk "") (chunk . T.pack . formatTime defaultTimeLocale "%F %H:%M" . utcToLocalTime tz) nextRowLastRun,
        maybe (chunk "") (chunk . T.pack . prettyTimeAuto (zonedTimeToUTC now)) nextRowLastRun,
        maybe (chunk "") (chunk . T.pack . formatTime defaultTimeLocale "%F %H:%M" . utcToLocalTime tz) nextRowNextRun,
        maybe (chunk "") (chunk . T.pack . prettyTimeAuto (zonedTimeToUTC now)) nextRowNextRun
      ]
