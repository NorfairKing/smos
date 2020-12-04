{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Report.TimeBlock where

import Control.DeepSeq
import Data.Aeson
import Data.Function
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Validity
import Data.Yaml.Builder (ToYaml (..))
import qualified Data.Yaml.Builder as Yaml
import GHC.Generics (Generic)
import Text.Printf

data TimeBlock
  = OneBlock
  | YearBlock
  | MonthBlock
  | WeekBlock
  | DayBlock
  deriving (Show, Eq, Generic)

instance Validity TimeBlock

instance NFData TimeBlock

instance FromJSON TimeBlock

instance ToJSON TimeBlock

data Block a b = Block
  { blockTitle :: a,
    blockEntries :: [b]
  }
  deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (Block a b)

instance (NFData a, NFData b) => NFData (Block a b)

instance (FromJSON a, FromJSON b) => FromJSON (Block a b) where
  parseJSON = withObject "Block" $ \o -> Block <$> o .: "title" <*> o .: "entries"

instance (ToJSON a, ToJSON b) => ToJSON (Block a b) where
  toJSON Block {..} = object ["title" .= blockTitle, "entries" .= blockEntries]

instance (ToYaml a, ToYaml b) => ToYaml (Block a b) where
  toYaml Block {..} = Yaml.mapping [("title", toYaml blockTitle), ("entries", toYaml blockEntries)]

mapBlockTitle :: (a -> b) -> Block a c -> Block b c
mapBlockTitle func b = b {blockTitle = func $ blockTitle b}

mapBlockEntries :: ([b] -> [c]) -> Block a b -> Block a c
mapBlockEntries func b = b {blockEntries = func $ blockEntries b}

divideIntoBlocks :: forall b. (b -> Day) -> TimeBlock -> [b] -> [Block Text b]
divideIntoBlocks func tb es =
  case tb of
    OneBlock -> [Block {blockTitle = "All Time", blockEntries = es}]
    YearBlock -> map (mapBlockTitle formatYearTitle) $ divideIntoTitledBlocks yearFunc es
    MonthBlock -> map (mapBlockTitle formatMonthTitle) $ divideIntoTitledBlocks monthFunc es
    WeekBlock -> map (mapBlockTitle formatWeekTitle) $ divideIntoTitledBlocks weekFunc es
    DayBlock -> map (mapBlockTitle formatDayTitle) $ divideIntoTitledBlocks func es
  where
    weekFunc :: b -> WeekNumber
    weekFunc = dayWeek . func
    monthFunc :: b -> MonthNumber
    monthFunc = dayMonth . func
    yearFunc :: b -> YearNumber
    yearFunc = dayYear . func

type YearNumber = Integer

dayYear :: Day -> YearNumber
dayYear = (\(y, _, _) -> y) . toGregorian

formatYearTitle :: YearNumber -> Text
formatYearTitle = T.pack . printf "%04d"

data MonthNumber = MonthNumber
  { monthNumberYear :: Integer,
    monthNumberMonth :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity MonthNumber where
  validate wn@MonthNumber {..} =
    mconcat
      [ genericValidate wn,
        declare "The month number is positive" $ monthNumberMonth >= 0,
        declare "The month number is less than or equal to 53" $ monthNumberMonth <= 53
      ]

instance Enum MonthNumber where
  toEnum i = dayMonth $ ModifiedJulianDay $ toEnum i
  fromEnum MonthNumber {..} =
    fromEnum $ toModifiedJulianDay $ fromGregorian monthNumberYear monthNumberMonth 1

dayMonth :: Day -> MonthNumber
dayMonth = (\(y, mn, _) -> MonthNumber {monthNumberYear = y, monthNumberMonth = mn}) . toGregorian

formatMonthTitle :: MonthNumber -> Text
formatMonthTitle MonthNumber {..} =
  T.pack $ concat [printf "%04d" monthNumberYear, "-", printf "%02d" monthNumberMonth]

data WeekNumber = WeekNumber
  { weekNumberYear :: Integer,
    weekNumberWeek :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity WeekNumber where
  validate wn@WeekNumber {..} =
    mconcat
      [ genericValidate wn,
        declare "The week number is positive" $ weekNumberWeek >= 0,
        declare "The week number is less than or equal to 53" $ weekNumberWeek <= 53
      ]

instance Enum WeekNumber where
  toEnum i = dayWeek $ ModifiedJulianDay $ toEnum i
  fromEnum WeekNumber {..} =
    fromEnum $ toModifiedJulianDay $ fromWeekDate weekNumberYear weekNumberWeek 1

dayWeek :: Day -> WeekNumber
dayWeek = (\(y, wn, _) -> WeekNumber {weekNumberYear = y, weekNumberWeek = wn}) . toWeekDate

formatWeekTitle :: WeekNumber -> Text
formatWeekTitle WeekNumber {..} =
  T.pack $ concat [printf "%04d" weekNumberYear, "-W", printf "%02d" weekNumberWeek]

formatDayTitle :: Day -> Text
formatDayTitle = T.pack . formatTime defaultTimeLocale "%F (%A)"

divideIntoTitledBlocks :: Ord t => (b -> t) -> [b] -> [Block t b]
divideIntoTitledBlocks func =
  sortOn blockTitle . combineBlocksByName . map (turnIntoSingletonBlock func)

turnIntoSingletonBlock :: (b -> t) -> b -> Block t b
turnIntoSingletonBlock func b = Block {blockTitle = func b, blockEntries = [b]}

combineBlocksByName :: Ord a => [Block a b] -> [Block a b]
combineBlocksByName = map comb . groupBy ((==) `on` blockTitle) . sortOn blockTitle
  where
    comb :: [Block a b] -> Block a b
    comb [] = error "cannot happen due to 'groupBy' above"
    comb bs@(b : _) = Block {blockTitle = blockTitle b, blockEntries = concatMap blockEntries bs}
