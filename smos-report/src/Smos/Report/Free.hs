{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Free
  ( produceFreeReport,
    freeReportConduit,
    FreeReport (..),
    produceFreeMap,
    freeMapConduit,
    FreeMap (..),
    Between (..),
    produceBusyMap,
    busyMapConduit,
    BusyMap (..),
    Slot (..),
    slotDuration,
    -- Internal
    entrySlot,
    mkBusyMap,
    busyMapToFreeMap,
    freeMapToFreeReport,
  )
where

import Conduit
import Control.Applicative
import qualified Data.Conduit.Combinators as C
import qualified Data.IntervalMap.Generic.Interval as IGI
import Data.IntervalMap.Generic.Lazy (IntervalMap)
import qualified Data.IntervalMap.Generic.Lazy as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import Data.Validity
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Path
import Smos.Data
import Smos.Directory.Archive
import Smos.Directory.OptParse
import Smos.Directory.ShouldPrint
import Smos.Directory.Streaming
import Smos.Report.Time

produceFreeReport ::
  (MonadIO m) =>
  HideArchive ->
  ShouldPrint ->
  DirectorySettings ->
  Slot ->
  Maybe Time ->
  Maybe TimeOfDay ->
  Maybe TimeOfDay ->
  m FreeReport
produceFreeReport ha sp ds slot mMinimumTime mEarliest mLatest =
  produceReport ha sp ds $
    freeReportConduit slot mMinimumTime mEarliest mLatest

freeReportConduit ::
  (Monad m) =>
  Slot ->
  Maybe Time ->
  Maybe TimeOfDay ->
  Maybe TimeOfDay ->
  ConduitT (Path Rel File, SmosFile) void m FreeReport
freeReportConduit slot mMinimumTime mEarliest mLatest =
  freeMapToFreeReport mEarliest mLatest
    <$> freeMapConduit slot mMinimumTime

produceFreeMap ::
  (MonadIO m) =>
  HideArchive ->
  ShouldPrint ->
  DirectorySettings ->
  Slot ->
  Maybe Time ->
  m FreeMap
produceFreeMap ha sp ds slot mMinimumTime =
  produceReport ha sp ds $
    freeMapConduit slot mMinimumTime

freeMapConduit ::
  (Monad m) =>
  Slot ->
  Maybe Time ->
  ConduitT (Path Rel File, SmosFile) void m FreeMap
freeMapConduit slot mMinimumTime =
  busyMapToFreeMap slot mMinimumTime <$> busyMapConduit

produceBusyMap ::
  (MonadIO m) =>
  HideArchive ->
  ShouldPrint ->
  DirectorySettings ->
  m BusyMap
produceBusyMap ha sp ds = produceReport ha sp ds busyMapConduit

busyMapConduit ::
  (Monad m) =>
  ConduitT (Path Rel File, SmosFile) void m BusyMap
busyMapConduit =
  fmap (mkBusyMap . IM.fromList) $
    smosFileEntries
      .| C.map snd
      .| C.concatMap (\e -> (,) <$> entrySlot e <*> pure e)
      .| C.sinkList

entrySlot ::
  Entry -> Maybe Slot
entrySlot e =
  let timestamps = entryTimestamps e
      properties = entryProperties e
      -- We assume that day-wide events are not busy by default, and
      -- specifically timed events are busy by default.
      defaultBusy = case M.lookup "BEGIN" timestamps <|> M.lookup "END" timestamps of
        Just (TimestampLocalTime _) -> True
        Just (TimestampDay _) -> False
        Nothing -> False -- Doesn't matter, won't find a slot anyway.
      busy = case M.lookup "busy" properties of
        Just "false" -> False
        Just "true" -> True
        _ -> defaultBusy
   in if busy
        then
          let mBeginTime = do
                beginTS <- M.lookup "BEGIN" timestamps
                case beginTS of
                  TimestampLocalTime lt -> Just lt
                  TimestampDay d -> Just $ LocalTime d midnight
              mEndTime = do
                endTS <- M.lookup "END" timestamps
                case endTS of
                  TimestampLocalTime lt -> Just lt
                  TimestampDay d -> Just $ LocalTime (addDays 1 d) midnight
           in case (,) <$> mBeginTime <*> mEndTime of
                Just (begin, end)
                  | begin <= end ->
                      Just Slot {slotBegin = begin, slotEnd = end}
                _ -> Nothing
        else Nothing

instance (Validity i, Validity v, IGI.Interval i k, Ord i) => Validity (IntervalMap i v) where
  validate im =
    mconcat
      [ declare "is valid according to IM.valid" $
          IM.valid im,
        decorateList (IM.toList im) $ \(i, v) ->
          mconcat
            [ delve "The interval" i,
              delve "The value" v
            ]
      ]

-- | A timeslot with a begin and end time
--
-- This is considered a closed-open interval.
data Slot = Slot
  { slotBegin :: !LocalTime,
    slotEnd :: !LocalTime
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Slot where
  validate s@Slot {..} =
    mconcat
      [ genericValidate s,
        declare "The begin is before the end" $ slotBegin <= slotEnd
      ]

instance IGI.Interval Slot LocalTime where
  lowerBound = slotBegin
  upperBound = slotEnd
  leftClosed = const True
  rightClosed = const False

slotLargeEnough :: Time -> Slot -> Bool
slotLargeEnough t Slot {..} =
  diffLocalTime slotEnd slotBegin
    >= timeNominalDiffTime t

slotDuration :: Slot -> NominalDiffTime
slotDuration Slot {..} = diffLocalTime slotEnd slotBegin

-- | A map of busy timeslots and events in them
--
-- Note that we must not use this map for the agenda
-- report because it does not contain events that
-- don't mark us as busy.
newtype BusyMap = BusyMap {unBusyMap :: IntervalMap Slot (IntervalMap Slot Entry)}
  deriving (Show, Eq, Generic)

instance Validity BusyMap where
  validate bm@(BusyMap im) =
    mconcat
      [ genericValidate bm,
        decorate "none of the slots are empty" $
          decorateList (IM.toList im) $ \(_, m) ->
            declare "The slot is not empty" $ not (IM.null m)
      ]

mkBusyMap :: IntervalMap Slot Entry -> BusyMap
mkBusyMap =
  BusyMap
    . IM.flattenWithMonotonic go
    . IM.mapWithKey IM.singleton
  where
    go ::
      (Slot, IntervalMap Slot Entry) ->
      (Slot, IntervalMap Slot Entry) ->
      Maybe (Slot, IntervalMap Slot Entry)
    go (s1, im1) (s2, im2) = do
      s <- slotCombine s1 s2
      pure (s, IM.union im1 im2)

slotCombine :: Slot -> Slot -> Maybe Slot
slotCombine s1 s2 =
  if IGI.overlaps s1 s2
    then
      Just $
        Slot
          (min (slotBegin s1) (slotBegin s2))
          (max (slotEnd s1) (slotEnd s2))
    else Nothing

-- | A map of free timeslots and the events around them
newtype FreeMap = FreeMap {unFreeMap :: IntervalMap Slot (Between Entry)}
  deriving (Show, Eq, Generic)

instance Validity FreeMap where
  validate fm@(FreeMap im) =
    mconcat
      [ genericValidate fm,
        decorateList (IM.keys im) $ \s ->
          declare "it is not a point slot" $
            slotBegin s /= slotEnd s
      ]

data Between a
  = Free
  | Before a
  | After a
  | Between a a
  deriving (Show, Eq, Generic)

instance (Validity a) => Validity (Between a)

busyMapToFreeMap :: Slot -> Maybe Time -> BusyMap -> FreeMap
busyMapToFreeMap intervalWeCareAbout mMinimumTime =
  if slotBegin intervalWeCareAbout >= slotEnd intervalWeCareAbout
    then const (FreeMap IM.empty)
    else
      FreeMap
        . IM.filterWithKey (\s _ -> slotBegin s < slotEnd s)
        . IM.filterWithKey
          ( \s _ ->
              maybe
                (const True)
                slotLargeEnough
                mMinimumTime
                s
          )
        . IM.fromAscList
        . go
        . IM.toAscList
        . (`IM.intersecting` intervalWeCareAbout)
        . unBusyMap
  where
    careLo = IGI.lowerBound intervalWeCareAbout
    careHi = IGI.upperBound intervalWeCareAbout
    go :: [(Slot, IntervalMap Slot Entry)] -> [(Slot, Between Entry)]
    go = \case
      [] -> [(intervalWeCareAbout, Free)]
      (t@(i, es) : ts) ->
        ( let firstLo = IGI.lowerBound i
           in [ ( Slot careLo firstLo,
                  Before (snd (IM.findMin es))
                )
                | firstLo >= careLo
              ]
        )
          ++ go2 (t : ts)
    go2 :: [(Slot, IntervalMap Slot Entry)] -> [(Slot, Between Entry)]
    go2 = \case
      [] -> []
      [(i, es)] ->
        let lastHi = IGI.upperBound i
         in [ ( Slot lastHi careHi,
                After (snd (IM.findLast es))
              )
              | lastHi <= careHi
            ]
      ((i1, es1) : u@(i2, es2) : ts) ->
        ( slotBetweenSlots i1 i2,
          Between (snd (IM.findLast es1)) (snd (IM.findMin es2))
        )
          : go2 (u : ts)

slotBetweenSlots :: Slot -> Slot -> Slot
slotBetweenSlots one two =
  Slot
    (IGI.upperBound one)
    (IGI.lowerBound two)

newtype FreeReport = FreeReport {unFreeReport :: Map Day FreeMap}
  deriving (Show, Eq, Generic)

instance Validity FreeReport

freeMapToFreeReport :: Maybe TimeOfDay -> Maybe TimeOfDay -> FreeMap -> FreeReport
freeMapToFreeReport mEarliest mLatest (FreeMap fm) = FreeReport $
  case (,) <$> IM.lookupMin fm <*> IM.lookupLast fm of
    Just ((firstSlot, _), (lastSlot, _)) ->
      let days = [localDay (slotBegin firstSlot) .. localDay (slotEnd lastSlot)]
       in M.filter (not . IM.null . unFreeMap) $
            M.fromList $
              flip map days $ \d ->
                ( d,
                  FreeMap
                    . IM.filterWithKey (\s _ -> slotBegin s < slotEnd s)
                    . IM.fromList
                    . mapMaybe (maybe Just (clampSlotToEarlierThan . LocalTime d) mLatest)
                    . mapMaybe (maybe Just (clampSlotToLaterThan . LocalTime d) mEarliest)
                    . map (clampSlotToDay d)
                    . IM.toList
                    $ IM.intersecting fm (daySlot d)
                )
    Nothing -> M.empty

daySlot :: Day -> Slot
daySlot d =
  Slot
    { slotBegin = LocalTime d midnight,
      slotEnd = LocalTime (addDays 1 d) midnight
    }

clampSlotToDay :: Day -> (Slot, Between Entry) -> (Slot, Between Entry)
clampSlotToDay d (s, b) =
  let oldBegin = slotBegin s
      oldEnd = slotEnd s
      newBegin = clampLocalTimeToDay d oldBegin
      newEnd = clampLocalTimeToDay d oldEnd
   in ( Slot
          { slotBegin = newBegin,
            slotEnd = newEnd
          },
        clampBetween (oldBegin /= newBegin) (oldEnd /= newEnd) b
      )

clampBetween :: Bool -> Bool -> Between a -> Between a
clampBetween beginClamped endClamped =
  (if beginClamped then clampBetweenBegin else id)
    . (if endClamped then clampBetweenEnd else id)

clampBetweenBegin :: Between a -> Between a
clampBetweenBegin = \case
  Free -> Free
  Before a -> Before a
  After _ -> Free
  Between _ a -> Before a

clampBetweenEnd :: Between a -> Between a
clampBetweenEnd = \case
  Free -> Free
  Before _ -> Free
  After a -> After a
  Between a _ -> After a

clampLocalTimeToDay :: Day -> LocalTime -> LocalTime
clampLocalTimeToDay d lt =
  let d' = localDay lt
   in case compare d' d of
        LT -> LocalTime d midnight
        EQ -> lt
        GT -> LocalTime (addDays 1 d) midnight

clampSlotToEarlierThan :: LocalTime -> (Slot, Between Entry) -> Maybe (Slot, Between Entry)
clampSlotToEarlierThan lt (s, b) =
  let oldBegin = slotBegin s
      oldEnd = slotEnd s
      newBegin = min lt oldBegin
      newEnd = min lt oldEnd
   in if newBegin < newEnd
        then
          Just
            ( Slot {slotBegin = newBegin, slotEnd = newEnd},
              if newEnd /= oldEnd then clampBetweenEnd b else b
            )
        else Nothing

clampSlotToLaterThan :: LocalTime -> (Slot, Between Entry) -> Maybe (Slot, Between Entry)
clampSlotToLaterThan lt (s, b) =
  let oldBegin = slotBegin s
      oldEnd = slotEnd s
      newBegin = max lt oldBegin
      newEnd = max lt oldEnd
   in if newBegin < newEnd
        then
          Just
            ( Slot {slotBegin = newBegin, slotEnd = newEnd},
              if newBegin /= oldBegin then clampBetweenBegin b else b
            )
        else Nothing
