{-# LANGUAGE OverloadedStrings #-}

module Smos.Draw.Base where

import Brick.Types as B
import Brick.Widgets.Core as B
import Data.Time
import Text.Printf

data Select
  = MaybeSelected
  | NotSelected
  deriving (Show, Eq)

instance Semigroup Select where
  MaybeSelected <> MaybeSelected = MaybeSelected
  _ <> _ = NotSelected

drawNominalDiffTime :: NominalDiffTime -> Widget n
drawNominalDiffTime ndt =
  hBox
    [ str $ printf "%.2d" hours,
      str ":",
      str $ printf "%.2d" minutes,
      str ":",
      str $ printf "%.2d" seconds
    ]
  where
    totalSeconds = round ndt :: Int
    totalMinutes = totalSeconds `div` secondsInAMinute
    totalHours = totalMinutes `div` minutesInAnHour
    secondsInAMinute = 60
    minutesInAnHour = 60
    hours = totalHours
    minutes = totalMinutes - minutesInAnHour * totalHours
    seconds = totalSeconds - secondsInAMinute * totalMinutes

formatTimestampDay :: Day -> String
formatTimestampDay = formatTime defaultTimeLocale "%A %F"

formatTimestampLocalTime :: LocalTime -> String
formatTimestampLocalTime = formatTime defaultTimeLocale "%A %F %R"
