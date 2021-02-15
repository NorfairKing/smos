{-# LANGUAGE OverloadedStrings #-}

module Smos.Draw.Base where

import Brick.Types as B
import Brick.Widgets.Core as B
import Cursor.Brick
import Cursor.Text
import Cursor.TextField
import Data.List
import Data.Time
import Path
import Smos.Data
import Smos.Style
import Smos.Types
import qualified System.FilePath as FP
import Text.Printf
import Text.Time.Pretty

data DrawEnv = DrawEnv
  { drawEnvWaitingThreshold :: !Word,
    drawEnvStuckThreshold :: !Word,
    drawEnvNow :: !ZonedTime
  }

type MDrawer = Reader DrawEnv (Maybe (Widget ResourceName))

type Drawer = Drawer' (Widget ResourceName)

type Drawer' = Reader DrawEnv

data Select
  = MaybeSelected
  | NotSelected
  deriving (Show, Eq)

instance Semigroup Select where
  MaybeSelected <> MaybeSelected = MaybeSelected
  _ <> _ = NotSelected

withHeading :: Widget n -> Widget n -> Widget n
withHeading hw w =
  vBox
    [ hBox [str "──[ ", withAttr selectedAttr hw, str " ]──", vLimit 1 $ fill '─'],
      w
    ]

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

listerChar :: Char
listerChar = ' '

pointerChar :: Char
pointerChar = '❯'

drawTable :: [[Widget n]] -> Widget n
drawTable = hBox . intersperse (str " ") . map vBox . transpose

drawHeader :: Header -> Widget n
drawHeader = withAttr headerAttr . textLineWidget . headerText

drawTodoState :: TodoState -> Widget n
drawTodoState ts =
  withAttr (todoStateSpecificAttr ts <> todoStateAttr) . textLineWidget $ todoStateText ts

drawTimestampName :: TimestampName -> Widget n
drawTimestampName tsn =
  withAttr (timestampNameSpecificAttr tsn <> timestampNameAttr) . textLineWidget $
    timestampNameText tsn

drawTimestamp :: Timestamp -> Widget n
drawTimestamp ts =
  case ts of
    TimestampDay d -> drawDay d
    TimestampLocalTime lt -> drawLocalTime lt

drawTimestampPrettyRelative :: Timestamp -> Drawer
drawTimestampPrettyRelative ts =
  case ts of
    TimestampDay d -> drawDayPrettyRelative d
    TimestampLocalTime lt -> drawLocalTimePrettyRelative lt

drawTimestampWithPrettyRelative :: Timestamp -> Drawer
drawTimestampWithPrettyRelative ts =
  case ts of
    TimestampDay d -> drawDayWithPrettyRelative d
    TimestampLocalTime lt -> drawLocalTimeWithPrettyRelative lt

drawDayWithPrettyRelative :: Day -> Drawer
drawDayWithPrettyRelative d = do
  prw <- drawDayPrettyRelative d
  pure $
    hBox
      [ str $ formatTimestampDay d,
        str ", ",
        prw
      ]

drawDay :: Day -> Widget n
drawDay d = str $ formatTimestampDay d

drawDayPrettyRelative :: Day -> Drawer
drawDayPrettyRelative d = do
  zt <- asks drawEnvNow
  pure $
    str $ prettyDayAuto (localDay $ zonedTimeToLocalTime zt) d

drawLocalTimeWithPrettyRelative :: LocalTime -> Drawer
drawLocalTimeWithPrettyRelative lt = do
  prw <- drawLocalTimePrettyRelative lt
  pure $
    hBox
      [ str $ formatTimestampLocalTime lt,
        str ", ",
        prw
      ]

drawLocalTime :: LocalTime -> Widget n
drawLocalTime lt = do
  str $ formatTimestampLocalTime lt

drawLocalTimePrettyRelative :: LocalTime -> Drawer
drawLocalTimePrettyRelative lt = do
  zt@(ZonedTime _ tz) <- asks drawEnvNow
  pure $
    str $ prettyTimeAuto (zonedTimeToUTC zt) $ localTimeToUTC tz lt

drawFilePath :: Path b File -> Widget n
drawFilePath fp =
  case fileExtension fp of
    Just ".smos" -> withAttr fileAttr . str . FP.dropExtension . toFilePath $ fp
    _ -> withAttr nonSmosFileAttr . str . toFilePath $ fp

drawDirPath :: Path b Dir -> Widget n
drawDirPath = withAttr dirAttr . str . toFilePath

drawTextFieldCursor :: Select -> TextFieldCursor -> Widget ResourceName
drawTextFieldCursor s =
  case s of
    MaybeSelected -> selectedTextFieldCursorWidget ResourceTextCursor
    _ -> textFieldCursorWidget

drawTextCursor :: Select -> TextCursor -> Widget ResourceName
drawTextCursor s =
  case s of
    MaybeSelected -> selectedTextCursorWidget ResourceTextCursor
    _ -> textCursorWidget
