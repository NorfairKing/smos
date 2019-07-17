{-# LANGUAGE OverloadedStrings #-}

module Smos.Draw.Base where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Path
import Text.Printf

import Brick.Types as B
import Brick.Widgets.Core as B

drawFilePath :: Path r d -> Widget n
drawFilePath = str . toFilePath

drawText :: Text -> Widget n
drawText = vBox . map go . T.splitOn "\n"
  where
    go t =
      txtWrap $
      case t of
        "" -> " "
        _ -> sanitise t
    sanitise =
      T.map $ \c ->
        case c of
          '\t' -> ' '
          _ -> c

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
    [ str $ printf "%.2d" hours
    , str ":"
    , str $ printf "%.2d" minutes
    , str ":"
    , str $ printf "%.2d" seconds
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
