{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Free (smosQueryFree) where

import Conduit
import qualified Data.IntervalMap.Generic.Lazy as IM
import qualified Data.Map as M
import qualified Data.Text as T
import Smos.CLI.Formatting
import Smos.Query.Commands.Import
import Smos.Report.Free
import Smos.Report.Period
import System.Exit

smosQueryFree :: FreeSettings -> Q ()
smosQueryFree FreeSettings {..} = do
  zone <- liftIO loadLocalTZ
  now <- liftIO getCurrentTime
  let today = localDay (utcToLocalTimeTZ zone now)

  sp <- getShouldPrint
  dc <- asks envDirectorySettings

  careSlot <- case periodInterval today freeSetPeriod of
    Interval begin end ->
      pure
        Slot
          { slotBegin = LocalTime begin midnight,
            slotEnd = LocalTime end midnight
          }
    _ -> liftIO $ die "Can only compute the free slots in a period that is bounded on both ends."
  freeReport <-
    produceFreeReport
      freeSetHideArchive
      sp
      dc
      careSlot
      freeSetMinimumTime
      freeSetEarliestTimeOfDay
      freeSetLatestTimeOfDay

  colourSettings <- asks envColourSettings
  outputChunks $ renderFreeReport colourSettings freeReport

renderFreeReport :: ColourSettings -> FreeReport -> [Chunk]
renderFreeReport colourSettings =
  intercalate ["\n"]
    . map (uncurry (renderFreeMapBlock colourSettings))
    . M.toList
    . unFreeReport

renderFreeMapBlock :: ColourSettings -> Day -> FreeMap -> [Chunk]
renderFreeMapBlock colourSettings day freeMap =
  fore blue (chunk $ T.pack $ formatTime defaultTimeLocale "%F: %A\n" day)
    : formatAsBicolourTable colourSettings (renderFreeMap freeMap)

renderFreeMap :: FreeMap -> [[Chunk]]
renderFreeMap =
  map (uncurry renderFreeTup)
    . IM.toAscList
    . unFreeMap

renderFreeTup :: Slot -> Between Entry -> [Chunk]
renderFreeTup Slot {..} b =
  [ fore green $ chunk $ T.pack $ formatTime defaultTimeLocale "%02H:%02M" $ localTimeOfDay slotBegin,
    fore green "-",
    fore green $ chunk $ T.pack $ formatTime defaultTimeLocale "%02H:%02M" $ localTimeOfDay slotEnd,
    fore magenta $ chunk $ T.pack $ formatTime defaultTimeLocale "%_2hh%02Mm" $ diffLocalTime slotEnd slotBegin,
    fore white $ case b of
      Free -> ""
      Before _ -> "before"
      After _ -> "after"
      Between _ _ -> "between",
    case b of
      Free -> ""
      Before e -> headerChunk (entryHeader e)
      After e -> headerChunk (entryHeader e)
      Between e _ -> headerChunk (entryHeader e),
    fore white $ case b of
      Between _ _ -> "and"
      _ -> "",
    case b of
      Between _ e -> headerChunk (entryHeader e)
      _ -> ""
  ]
