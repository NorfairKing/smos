{-# LANGUAGE OverloadedStrings #-}

module Smos.Draw.Base where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Path
import Text.Printf

import Brick.Types as B
import Brick.Widgets.Core as B

import Graphics.Vty.Input.Events (Key(..), Modifier(..))

drawFilePath :: Path r d -> Widget n
drawFilePath = str . toFilePath

drawText :: Text -> Widget n
drawText = vBox . map go . T.splitOn "\n"
  where
    go t =
      txtWrap $
      case t of
        "" -> " "
        _ -> t

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

showKey :: Key -> String
showKey (KChar '\t') = "<tab>"
showKey (KChar ' ') = "<space>"
showKey (KChar c) = [c]
showKey KBackTab = "S-<tab>"
showKey (KFun i) = "F" ++ show i
showKey k = go $ show k
    -- Because these constructors all start with 'K'
  where
    go [] = []
    go ('K':s) = s
    go s = s

showMod :: Modifier -> String
showMod MShift = "S"
showMod MCtrl = "C"
showMod MMeta = "M"
showMod MAlt = "A"
