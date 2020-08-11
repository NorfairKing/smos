{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.RecurrenceRule.Recurrence.Util where

import Data.Fixed
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import Data.Time.Calendar.MonthDay
import Debug.Trace
import Smos.Calendar.Import.RecurrenceRule.Type

iterateMaybeSet :: Ord a => (a -> Maybe a) -> a -> Set a
iterateMaybeSet func start = go start
  where
    go cur = case func cur of
      Nothing -> S.singleton start
      Just next -> S.insert next $ go next

filterEvery :: Set ByDay -> Set DayOfWeek
filterEvery =
  S.fromList
    . mapMaybe
      ( \case
          Every d -> Just d
          _ -> Nothing
      )
    . S.toList

takeEvery :: Word -> [a] -> [a]
takeEvery i = go 0
  where
    go _ [] = []
    go 0 (l : ls) = l : go (i -1) ls
    go j (_ : ls) = go (pred j) ls

byMonthLimitMonth :: Set ByMonth -> Month -> Bool
byMonthLimitMonth = limitBy $ \m1 m2 -> m1 == m2

byMonthLimit :: Set ByMonth -> Day -> Bool
byMonthLimit = limitBy $ \d m ->
  let (_, month, _) = toGregorian d
   in month == monthToMontNo m

byMonthDayLimit :: Set ByMonthDay -> Day -> Bool
byMonthDayLimit = limitBy $ \d (MonthDay md) ->
  let (positiveMonthDayIndex, negativeMonthDayIndex) = monthIndices d
   in positiveMonthDayIndex == md
        || negativeMonthDayIndex == md

monthIndices :: Day -> (Int, Int) -- (Positive index, Negative index)
monthIndices d =
  let (y, month, day) = toGregorian d
      leap = isLeapYear y
      monthLen = monthLength leap month
      negativeMonthDayIndex = negate $ monthLen - day + 1
   in (day, negativeMonthDayIndex)

byDayLimit :: Set ByDay -> Day -> Bool
byDayLimit = limitBy $ \d bd -> case bd of
  Every dow -> dayOfWeek d == dow
  Specific i dow ->
    let (pos, neg) = specificWeekDayIndex d dow
     in dayOfWeek d == dow && (i == pos || i == neg)

byEveryWeekDayLimit :: Set DayOfWeek -> Day -> Bool
byEveryWeekDayLimit = limitBy $ \d dow -> dow == dayOfWeek d

byDayExpand :: Day -> Set ByDay -> [Day]
byDayExpand d_ = flip expandL d_ $ \bd ->
  let (y, m, _) = toGregorian d_
      qdrups = daysOfMonth y m
   in case bd of
        Every dow ->
          mapMaybe
            ( \(d, _, _, dow') ->
                if dow == dow' then Just d else Nothing
            )
            qdrups
        Specific i dow ->
          mapMaybe
            ( \(d, p, n, dow') ->
                if dow == dow' && (i == p || i == n) then Just d else Nothing
            )
            qdrups

byMonthDayExpand :: Integer -> Month -> Int -> Set ByMonthDay -> [Int]
byMonthDayExpand y m = expandM $ \(MonthDay md) ->
  let len = monthLength (isLeapYear y) (monthToMontNo m)
   in case compare md 0 of
        EQ -> Nothing -- Should not happen
        LT ->
          -- Negative
          Just $ len - md
        GT -> Just md

byEveryWeekDayExpand :: DayOfWeek -> Set DayOfWeek -> [DayOfWeek]
byEveryWeekDayExpand = expand id

timeOfDayExpand :: TimeOfDay -> Set ByHour -> Set ByMinute -> Set BySecond -> [TimeOfDay]
timeOfDayExpand (TimeOfDay h_ m_ s_) byHours byMinutes bySeconds = do
  h <- byHourExpand h_ byHours
  m <- byMinuteExpand m_ byMinutes
  s <- bySecondExpand s_ bySeconds
  let tod = TimeOfDay h m s
  pure tod

byHourExpand :: Int -> Set ByHour -> [Int]
byHourExpand = expand (fromIntegral . unHour)

byMinuteExpand :: Int -> Set ByMinute -> [Int]
byMinuteExpand = expand (fromIntegral . unMinute)

bySecondExpand :: Pico -> Set BySecond -> [Pico]
bySecondExpand = expand (fromIntegral . unSecond)

expand :: (b -> a) -> a -> Set b -> [a]
expand func def bys = if S.null bys then pure def else map func (S.toList bys)

expandM :: (b -> Maybe a) -> a -> Set b -> [a]
expandM func def bys = if S.null bys then pure def else mapMaybe func (S.toList bys)

expandL :: (b -> [a]) -> a -> Set b -> [a]
expandL func def bys = if S.null bys then pure def else concatMap func (S.toList bys)

filterSetPos :: Set BySetPos -> [a] -> [a]
filterSetPos poss values =
  map snd (filter (limitBy func poss) (zip [1 ..] values))
  where
    len = length values
    func (i, _) (SetPos pos) =
      let toNegative positive = negate $ len - positive + 1
       in i == pos || toNegative i == pos

limitBy :: (b -> a -> Bool) -> Set a -> b -> Bool
limitBy func bys b =
  if S.null bys
    then True
    else any (func b) bys

-- This can probably be sped up a lot using the weekdate module
specificWeekDayIndex :: Day -> DayOfWeek -> (Int, Int) -- (Positive index, Negative index)
specificWeekDayIndex d wd =
  let (y, month, _) = toGregorian d
      firstDayOfTheMonth = fromGregorian y month 1
      lastDayOfTheMonth = fromGregorian y month 31 -- Will be clipped
      daysOfThisMonth = numberWeekdays [firstDayOfTheMonth .. lastDayOfTheMonth]
      numberOfThisWeekDayInTheMonth = length $ filter ((== wd) . fst . snd) daysOfThisMonth
      (_, positiveSpecificWeekDayIndex) = fromJust (lookup d daysOfThisMonth) -- Must be there
   in (positiveSpecificWeekDayIndex, negate $ numberOfThisWeekDayInTheMonth - positiveSpecificWeekDayIndex + 1)

-- Quadruples: Day, Positive index, negative index, day of week
daysOfMonth :: Integer -> Int -> [(Day, Int, Int, DayOfWeek)]
daysOfMonth year month = map go daysOfThisMonth
  where
    firstDayOfTheMonth = fromGregorian year month 1
    lastDayOfTheMonth = fromGregorian year month 31 -- Will be clipped
    days = [firstDayOfTheMonth .. lastDayOfTheMonth]
    daysOfThisMonth = numberWeekdays days
    numberOfThisWeekDayInTheMonth wd = fromMaybe 1 $ M.lookup wd $ count $ map dayOfWeek days
    go :: (Day, (DayOfWeek, Int)) -> (Day, Int, Int, DayOfWeek)
    go (d, (dow, p)) =
      let n = negate $ fromIntegral (numberOfThisWeekDayInTheMonth dow) - p + 1
       in (d, p, n, dow)

numberWeekdays :: [Day] -> [(Day, (DayOfWeek, Int))]
numberWeekdays = go M.empty
  where
    go _ [] = []
    go m (d_ : ds) =
      let dow = dayOfWeek d_
          (mv, m') =
            M.insertLookupWithKey
              (\_ new old -> new + old) -- If found, just increment
              dow
              1 -- If not found, insert 1
              m
       in (d_, (dow, maybe 1 succ mv)) : go m' ds

count :: forall a. Ord a => [a] -> Map a Word
count = foldl go M.empty
  where
    go :: Map a Word -> a -> Map a Word
    go m a = M.alter go2 a m
    go2 :: Maybe Word -> Maybe Word
    go2 Nothing = Just 1
    go2 (Just i) = Just $ i + 1
