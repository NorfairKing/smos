{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Sizing.Command.Report where

import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Tree
import Smos.Data
import Smos.Report.Time
import Smos.Sizing.OptParse.Types
import System.Exit
import Text.Colour
import Text.Colour.Term

sizingReport :: Settings -> ReportSettings -> IO ()
sizingReport Settings ReportSettings {..} = do
  mErrOrSmosFile <- readSmosFile reportSettingPlanFile
  case mErrOrSmosFile of
    Nothing -> die $ unwords ["File does not exist:", show reportSettingPlanFile]
    Just (Left err) -> die $ unlines ["Could not read plan file:", err]
    Just (Right sf) -> do
      let rawDays = computeTotalRawDays sf

      putChunksLocale $ concatMap (<> ["\n"]) $ renderReport rawDays

computeTotalRawDays :: SmosFile -> Word
computeTotalRawDays =
  round
    . foldl' (+) 0
    . mapMaybe go
    . concatMap flatten
    . smosFileForest
  where
    go :: Entry -> Maybe Double
    go e = do
      -- TODO: Disambiguate months and minutse
      -- TODO: Complain if an estimate fails to parse.
      epv <- M.lookup "estimate" (entryProperties e)
      t <- time (propertyValueText epv)
      pure $ timeDays t

timeDays :: Time -> Double
timeDays = \case
  Seconds s -> timeDays (Minutes (s `div` 60))
  Minutes s -> timeDays (Hours (s `div` 60))
  Hours s -> timeDays (Days (s `div` 24))
  Days d -> fromIntegral d
  Weeks w -> timeDays (Days (w * daysPerWeek))
  Months m -> timeDays (Weeks (m * weeksPerMonth))
  Years y -> timeDays (Months (y * monthsPerYear))

weeks :: Word -> Word
weeks days =
  ceiling $
    fromIntegral days
      / (fromIntegral daysPerWeek :: Double)

months :: Word -> Word
months days =
  ceiling $
    fromIntegral days
      / fromIntegral daysPerWeek
      / (fromIntegral weeksPerMonth :: Double)

years :: Word -> Word
years days =
  ceiling $
    fromIntegral days
      / fromIntegral daysPerWeek
      / fromIntegral weeksPerMonth
      / (fromIntegral monthsPerYear :: Double)

monthsPerYear :: Word
monthsPerYear = 12

weeksPerMonth :: Word
weeksPerMonth = 4

daysPerWeek :: Word
daysPerWeek = 5

renderReport :: Word -> [[Chunk]]
renderReport rawDays =
  let mulitplierLo = 2
      mulitplierHi = 4
      loDays = mulitplierLo * rawDays
      hiDays = mulitplierHi * rawDays
   in [ fore blue "Low:  " : renderDays loDays,
        fore blue "High: " : renderDays hiDays
      ]

renderDays :: Word -> [Chunk]
renderDays d
  | weeks d <= 2 =
    [ fore green $ chunk (T.pack (show d)),
      " ",
      fore green "person-days"
    ]
  | months d <= 2 =
    [ fore yellow $ chunk (T.pack (show (weeks d))),
      " ",
      fore yellow "person-weeks"
    ]
  | years d <= 2 =
    let orange :: Colour
        orange = color256 166
     in [ fore orange $ chunk (T.pack (show (months d))),
          " ",
          fore orange "person-months"
        ]
  | otherwise =
    [ fore red $ chunk (T.pack (show (years d))),
      " ",
      fore red "person-years"
    ]
