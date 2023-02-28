{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Sizing.Command.Report (sizingReport) where

import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Data.Tree
import Smos.Data
import Smos.Report.Time
import Smos.Sizing.OptParse.Types
import System.Exit

sizingReport :: Settings -> ReportSettings -> IO ()
sizingReport Settings ReportSettings {..} = do
  mErrOrSmosFile <- readSmosFile reportSettingPlanFile
  case mErrOrSmosFile of
    Nothing -> die $ unwords ["File does not exist:", show reportSettingPlanFile]
    Just (Left err) -> die $ unlines ["Could not read plan file:", err]
    Just (Right sf) -> do
      let rawDays = computeTotalRawDays sf

      putStrLn (renderReport rawDays)

renderReport :: Word -> String
renderReport rawDays =
  let mulitplierLo = 2
      mulitplierHi = 4
      loDays = mulitplierLo * rawDays
      hiDays = mulitplierHi * rawDays
   in unlines
        [ unwords ["Low: ", renderDays loDays],
          unwords ["High:", renderDays hiDays]
        ]

renderDays :: Word -> String
renderDays d
  | weeks d <= 2 =
    unwords
      [ show d,
        "person-days"
      ]
  | months d <= 2 =
    unwords
      [ show (weeks d),
        "person-weeks"
      ]
  | otherwise =
    unwords
      [ show (months d),
        "person-months"
      ]

weeks :: Word -> Word
weeks days = ceiling $ fromIntegral days / (fromIntegral daysPerWeek :: Double)

months :: Word -> Word
months days = ceiling $ fromIntegral days / fromIntegral daysPerWeek / (fromIntegral weeksPerMonth :: Double)

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

monthsPerYear :: Word
monthsPerYear = 12

weeksPerMonth :: Word
weeksPerMonth = 4

daysPerWeek :: Word
daysPerWeek = 5
