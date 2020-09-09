{-# LANGUAGE OverloadedStrings #-}

module Smos.Draw.Base where

import Brick.Types as B
import Brick.Widgets.Core as B
import Cursor.Brick
import Cursor.List.NonEmpty
import Cursor.Text
import Cursor.TextField
import Data.List
import Data.Time
import Path
import Smos.Data
import Smos.Style
import Smos.Types
import Text.Printf

type DrawEnv = ZonedTime

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
listerChar = '-'

pointerChar :: Char
pointerChar = '❯'

verticalNonEmptyCursorTable ::
  (b -> [Widget n]) -> (a -> [Widget n]) -> (b -> [Widget n]) -> NonEmptyCursor a b -> Widget n
verticalNonEmptyCursorTable prevFunc curFunc nextFunc =
  nonEmptyCursorWidget (\ps c ns -> drawTable $ map prevFunc ps ++ [curFunc c] ++ map nextFunc ns)

drawTable :: [[Widget n]] -> Widget n
drawTable = hBox . intersperse (str " ") . map vBox . transpose

drawHeader :: Header -> Widget n
drawHeader = withAttr headerAttr . textLineWidget . headerText

drawTodoState :: TodoState -> Widget n
drawTodoState ts =
  withAttr (todoStateSpecificAttr ts <> todoStateAttr) . textLineWidget $ todoStateText ts

drawFilePath :: Path b File -> Widget n
drawFilePath = withAttr fileAttr . str . toFilePath

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
