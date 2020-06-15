{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import where

import Control.Monad
import Control.Monad.Reader
import Data.Default
import Data.Function
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Lens.Micro
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import Path
import Smos.Calendar.Import.OptParse
import Smos.Data
import Smos.Report.Config
import System.Exit
import Text.ICalendar.Parser
import Text.ICalendar.Types as ICal
import Text.Show.Pretty

-- Given a list of sources (basically ics files) we want to produce a single smos file (like calendar.smos) that contains all the events.
-- For each event we want an entry with the description in the header.
--
-- Events can recur. We will put each entry in its own tree.
-- For each recurrence, we will put the recurring entries in the subforest of that tree.
-- For recurrence too far into the future we don't want to create any entries.
--
-- To do the import, we will first gather all the timezone information by their id: 'vcTimeZones'.
--
-- Then we will parse each event.
-- Things can go wrong during parsing of the events, but we must not miss any information, so we will produce warnings if anything in the results could be wrong.
-- Each event can have up to two timestamps: maybe a begin and maybe an end.
-- An event can be:
--
-- - relative: only a local time (no timezone), this will trigger a warning
-- - absolute: only a utctime, this is great
-- - rooted: a localtime plus a timezone
--
-- Then we need to produce the recurrences (before we resolve the localtimes).
--
-- Once we've got got the first event and all recurrences, we can resolve the localtimes to localtimes in the current timezone.

smosCalendarImport :: IO ()
smosCalendarImport = do
  Settings {..} <- getSettings
  today <- utctDay <$> getCurrentTime
  let start = addDays (-7) today
  let recurrenceLimit = addDays 30 today
  hereTZ <- getCurrentTimeZone
  fs <- forM (NE.toList setSources) $ \source -> do
    errOrCal <- case parseAbsFile source of
      Nothing -> do
        req <- parseRequest source
        putStrLn $ "Fetching: " <> source
        resp <- httpLBS req
        let errOrCal = parseICalendar def source $ getResponseBody resp
        pure errOrCal
      Just af -> parseICalendarFile def $ fromAbsFile af
    case errOrCal of
      Left err -> die err
      Right (cals, warnings) -> do
        forM_ warnings $ \warning -> putStrLn $ "WARNING: " <> warning
        pure $ processCalendars setDebug start recurrenceLimit hereTZ source cals
  wd <- resolveDirWorkflowDir setDirectorySettings
  let fp = wd </> setDestinationFile
  writeSmosFile fp $ SmosFile $ sorter fs
  where
    sorter = sortOn (entryTimestamps . rootLabel)

processCalendars :: Bool -> Day -> Day -> TimeZone -> String -> [VCalendar] -> Tree Entry
processCalendars debug start recurrenceLimit hereTZ name vcals = titleNode $ concatMap goCal vcals
  where
    titleNode :: Forest Entry -> Tree Entry
    titleNode = Node ((newEntry titleHeader) {entryContents = titleContents})
    titleHeader = fromMaybe "Invalid title name" $ header (T.pack name)
    titleContents =
      if debug
        then contents $ T.pack $ ppShow vcals
        else Nothing
    goCal :: VCalendar -> Forest Entry
    goCal cal =
      let env =
            ImportEnv
              { importEnvTimeZone = hereTZ,
                importEnvStartDay = start,
                importEnvRecurrentLimit = recurrenceLimit,
                importEnvNamedTimeZones = vcTimeZones cal,
                importEnvDebug = debug
              }
       in runReader (processEvents (vcEvents cal)) env

data ImportEnv
  = ImportEnv
      { importEnvStartDay :: !Day,
        importEnvRecurrentLimit :: !Day, -- TODO make this configurable
        importEnvTimeZone :: !TimeZone,
        importEnvNamedTimeZones :: !(Map Lazy.Text VTimeZone),
        importEnvDebug :: !Bool
      }
  deriving (Show, Eq)

type I = Reader ImportEnv

processEvents :: Map (Lazy.Text, Maybe (Either Date DateTime)) VEvent -> I [Tree Entry]
processEvents m = fmap catMaybes $ mapM (\(_, v) -> processEvent v) $ M.toList m

processEvent :: VEvent -> I (Maybe (Tree Entry))
processEvent ve = do
  let mDesc = eventDescription ve -- We must not miss any time
  mContents <- eventContents ve
  let firstTimestamps = eventFirstTimestamps ve
  recurrenceTimestamps <- processRecurrences (S.map rRuleValue $ veRRule ve) firstTimestamps
  resolvedFirstTimestamps <- resolveEventTimestamps firstTimestamps
  resolvedRecurrenceTimestamps <- mapM resolveEventTimestamps $ removeRecurrenceExceptions (veExDate ve) recurrenceTimestamps
  pruneOldTree $ Node (produceEntry mDesc mContents resolvedFirstTimestamps) (map (toNode . produceEntry mDesc Nothing) resolvedRecurrenceTimestamps)

eventDescription :: VEvent -> Maybe Text
eventDescription = fmap (Lazy.toStrict . summaryValue) . veSummary

eventContents :: VEvent -> I (Maybe Contents)
eventContents ve = do
  debug <- asks importEnvDebug
  pure $
    if debug
      then contents $ T.pack $ ppShow ve
      else (descriptionValue <$> veDescription ve) >>= (contents . Lazy.toStrict)

data EventTimestamps
  = EventTimestamps
      { eventTimestampsStart :: Maybe StartTime,
        eventTimestampsEnd :: Maybe EndTime
      }
  deriving (Show, Eq)

data StartTime
  = StartDate Day
  | StartDateTime DateTime
  deriving (Show, Eq)

startTimeDayL :: Lens' StartTime Day
startTimeDayL =
  lens
    ( \case
        StartDate d -> d
        StartDateTime vdt -> vdt ^. dateTimeDayL
    )
    ( \st d -> case st of
        StartDate _ -> StartDate d
        StartDateTime vdt -> StartDateTime $ vdt & dateTimeDayL .~ d
    )

dateTimeDayL :: Lens' DateTime Day
dateTimeDayL =
  lens
    ( \case
        FloatingDateTime (LocalTime d _) -> d
        UTCDateTime (UTCTime d _) -> d
        ZonedDateTime (LocalTime d _) _ -> d
    )
    ( \dt d -> case dt of
        FloatingDateTime (LocalTime _ tod) -> FloatingDateTime (LocalTime d tod)
        UTCDateTime (UTCTime _ dt') -> UTCDateTime (UTCTime d dt')
        ZonedDateTime (LocalTime _ tod) tzn -> ZonedDateTime (LocalTime d tod) tzn
    )

-- This is meant to be (<=)
-- I.e, this should be _true_ if the given timestamp should still be in the recurrence series
-- of which the second argument is the next ocurrence and the first is the limit.
eventTimestampsBefore :: StartTime -> EventTimestamps -> Bool
eventTimestampsBefore st ets = case eventTimestampsStart ets of
  Nothing -> case eventTimestampsEnd ets of
    Nothing -> True -- Just to be safe
    Just et -> case et of
      EndDuration _ -> True -- Just to be safe
      EndTimeStart st' -> go st'
  Just st' -> go st'
  where
    go :: StartTime -> Bool
    go st' = ((<=) `on` (\t -> t ^. startTimeDayL)) st st' -- TODO This might make an event too much?

data EndTime
  = EndTimeStart StartTime -- It's not a start time but it has the same idea
  | EndDuration Duration
  deriving (Show, Eq)

type TimezoneId = Text

eventFirstTimestamps :: VEvent -> EventTimestamps
eventFirstTimestamps VEvent {..} =
  let eventTimestampsStart = startTime <$> veDTStart
      eventTimestampsEnd = endTime <$> veDTEndDuration
   in EventTimestamps {..}

endTime :: Either DTEnd DurationProp -> EndTime
endTime = \case
  Left (DTEndDate (Date d) _) -> EndTimeStart (StartDate d)
  Left (DTEndDateTime dt _) -> EndTimeStart (StartDateTime dt)
  Right (DurationProp d _) -> EndDuration d

startTime :: DTStart -> StartTime
startTime = \case
  DTStartDate (Date d) _ -> StartDate d
  DTStartDateTime dt _ -> StartDateTime dt

processRecurrences :: Set Recur -> EventTimestamps -> I [EventTimestamps]
processRecurrences recur ets = concat <$> mapM (`processRecurrence` ets) (S.toList recur)

processRecurrence :: Recur -> EventTimestamps -> I [EventTimestamps]
processRecurrence = go
  where
    go :: Recur -> EventTimestamps -> I [EventTimestamps]
    go recur tsr = case recurUntilCount recur of
      Nothing -> goInfinite tsr
      Just (Right c) -> goCount c tsr
      Just (Left (Left d)) -> goUntilDate d tsr
      Just (Left (Right dt)) -> goUntilDateTime dt tsr
      where
        goInfinite tsr_ = do
          mNext <- nextRecurrence recur tsr_
          case mNext of
            Nothing -> pure []
            Just next -> do
              rest <- goInfinite next
              pure $ next : rest
        goCount c tsr_
          | c <= 0 = pure []
          | otherwise = do
            mNext <- nextRecurrence recur tsr_
            case mNext of
              Nothing -> pure []
              Just next -> do
                rest <- goCount (c - 1) next
                pure $ next : rest
        goUntilDate (Date d) tsr_ = do
          startDay <- asks importEnvStartDay
          if d < startDay -- The last recurrence is before the start date
            then pure []
            else do
              mNext <- nextRecurrence recur tsr_
              case mNext of
                Nothing -> pure []
                Just next ->
                  if eventTimestampsBefore (StartDate d) next
                    then do
                      rest <- goUntilDate (Date d) next
                      pure $ next : rest
                    else pure []
        goUntilDateTime dt tsr_ = do
          startDay <- asks importEnvStartDay
          if dt ^. dateTimeDayL < startDay -- The last recurrence is before the start date
            then pure []
            else do
              mNext <- nextRecurrence recur tsr_
              case mNext of
                Nothing -> pure []
                Just next ->
                  if eventTimestampsBefore (StartDateTime dt) next
                    then do
                      rest <- goUntilDateTime dt next
                      pure $ next : rest
                    else pure []

nextRecurrence :: Recur -> EventTimestamps -> I (Maybe EventTimestamps)
nextRecurrence recur ets = do
  nst <- forM (eventTimestampsStart ets) (nextStartTime recur)
  net <- forM (eventTimestampsEnd ets) (nextEndTime recur)
  pure $ EventTimestamps <$> nst <*> net

nextEndTime :: Recur -> EndTime -> I (Maybe EndTime)
nextEndTime recur = \case
  EndTimeStart st -> fmap EndTimeStart <$> nextStartTime recur st
  EndDuration d -> pure $ Just $ EndDuration d

nextStartTime :: Recur -> StartTime -> I (Maybe StartTime)
nextStartTime recur@Recur {..} st = case recurFreq of
  Yearly -> nextYearlyRecurrence recurInterval recurByYearDay st
  Weekly -> nextWeeklyRecurrence recurInterval recurByDay st
  Daily -> nextDailyRecurrence recurInterval recurByHour st
  _ -> error $ "Unsupported recurrence: " <> ppShow recur

nextYearlyRecurrence :: Int -> [Int] -> StartTime -> I (Maybe StartTime)
nextYearlyRecurrence recurInterval recurByYearDay st = case recurByYearDay of
  [] ->
    let func d =
          let (y, yd) = toOrdinalDate d
           in fromOrdinalDate (y + fromIntegral recurInterval) yd
     in limitStartTime $ st & startTimeDayL %~ func
  _ -> error "Unsupported yearly recurrence"

nextWeeklyRecurrence :: Int -> [Either (Int, Weekday) Weekday] -> StartTime -> I (Maybe StartTime)
nextWeeklyRecurrence recurInterval recurByDay st =
  case recurByDay of
    [] -> limitStartTime $ st & startTimeDayL %~ addDays 7
    _ ->
      let nextDayFuncs = map (either (\(i, wd) -> nextWeeklyByNumberedDayOfWeek recurInterval i wd) (\wd -> nextWeeklyByDayOfWeek recurInterval wd)) recurByDay :: [Day -> Day]
          nextStartTimes = map (\f -> st & startTimeDayL %~ f) nextDayFuncs :: [StartTime]
       in case nextStartTimes of
            [] -> pure Nothing
            (st' : _) -> limitStartTime st'

nextWeeklyByDayOfWeek ::
  -- | Interval
  Int ->
  Weekday ->
  Day ->
  Day
nextWeeklyByDayOfWeek interval _ day =
  if interval <= 0
    then error "Negative interval"
    else
      let baseDay = addDays (fromIntegral $ (interval - 1) * 7) day
       in go baseDay
  where
    -- Until we get time 1.9
    dayOfWeek d = let (_, _, dow) = toWeekDate d in dow
    go d =
      let d' = addDays 1 d
       in if dayOfWeek d' == dayOfWeek day
            then d'
            else go d'

nextWeeklyByNumberedDayOfWeek ::
  -- | Interval
  Int ->
  -- Numbered weekday within the month
  Int ->
  Weekday ->
  Day ->
  Day
nextWeeklyByNumberedDayOfWeek _ _ _ _ = error "Unsupported recurrence: nextWeeklyByNumberedDayOfWeek"

nextDailyRecurrence ::
  -- | Interval
  Int ->
  [Int] ->
  StartTime ->
  I (Maybe StartTime)
nextDailyRecurrence _ recurByHour st = case recurByHour of
  [] -> limitStartTime (st & startTimeDayL %~ addDays 1)
  _ -> error "Unsupported recurrence: nextDailyRecurrence"

limitStartTime :: StartTime -> I (Maybe StartTime)
limitStartTime st = do
  limitDay <- asks importEnvRecurrentLimit
  pure $
    if st ^. startTimeDayL > limitDay
      then Nothing
      else Just st

removeRecurrenceExceptions ::
  Set ExDate -> [EventTimestamps] -> [EventTimestamps]
removeRecurrenceExceptions exDates = filter (not . matchesAnyExDate exDates)

matchesAnyExDate :: Set ExDate -> EventTimestamps -> Bool
matchesAnyExDate s ets = any (`matchesEx` ets) s

matchesEx :: ExDate -> EventTimestamps -> Bool
matchesEx ed ets = case ed of
  ExDates ds _ -> any (`matchesExDate` ets) (S.map dateValue ds)
  ExDateTimes dts _ -> any (`matchesExDateTime` ets) dts

matchesExDate :: Day -> EventTimestamps -> Bool
matchesExDate d ets = case eventTimestampsStart ets of
  Nothing -> False
  Just st -> st ^. startTimeDayL == d

matchesExDateTime :: DateTime -> EventTimestamps -> Bool
matchesExDateTime dt ets = case eventTimestampsStart ets of
  Nothing -> False
  Just sd -> sd == StartDateTime dt

data TimestampsResult
  = TimestampsResult
      { timestampsResultBegin :: !TimestampResult,
        timestampsResultEnd :: !TimestampResult
      }
  deriving (Show, Eq)

data TimestampResult
  = TimestampResult
      { timestampResultTimestamp :: Maybe Timestamp,
        timestampResultWarning :: Maybe TimestampWarning
      }
  deriving (Show, Eq)

data TimestampWarning
  = TimestampNoTimezone
  | TimestampUnknownTimezone Lazy.Text
  deriving (Show, Eq)

resolveEventTimestamps :: EventTimestamps -> I TimestampsResult
resolveEventTimestamps EventTimestamps {..} = do
  timestampsResultBegin <- resolveMStartTime eventTimestampsStart
  timestampsResultEnd <- resolveMEndTime timestampsResultBegin eventTimestampsEnd
  pure TimestampsResult {..}

resolveMStartTime :: Maybe StartTime -> I TimestampResult
resolveMStartTime Nothing = pure $ TimestampResult Nothing Nothing
resolveMStartTime (Just st) = case st of
  StartDate d -> pure $ TimestampResult (Just (TimestampDay d)) Nothing
  StartDateTime dt -> resolveDateTime dt

resolveMEndTime :: TimestampResult -> Maybe EndTime -> I TimestampResult
resolveMEndTime _ Nothing = pure $ TimestampResult Nothing Nothing
resolveMEndTime startResult (Just end) = case end of
  EndTimeStart st -> resolveMStartTime (Just st)
  EndDuration dur -> case timestampResultTimestamp startResult of
    Nothing -> pure $ TimestampResult Nothing Nothing
    Just start -> resolveEndDuration start dur

resolveDateTime :: DateTime -> I TimestampResult
resolveDateTime = \case
  FloatingDateTime lt -> pure $ TimestampResult (Just (TimestampLocalTime lt)) (Just TimestampNoTimezone)
  UTCDateTime utct -> do
    hereTZ <- asks importEnvTimeZone
    pure $ TimestampResult (Just (TimestampLocalTime (utcToLocalTime hereTZ utct))) Nothing
  ZonedDateTime lt tzn -> do
    hereTZ <- asks importEnvTimeZone
    namedTimezones <- asks importEnvNamedTimeZones
    case M.lookup tzn namedTimezones of
      Nothing -> pure $ TimestampResult (Just (TimestampLocalTime lt)) (Just (TimestampUnknownTimezone tzn))
      Just vtz -> pure $ TimestampResult (Just (TimestampLocalTime (resolveLocalTimeWithVTimeZone lt vtz hereTZ))) Nothing

resolveLocalTimeWithVTimeZone ::
  -- | Local time in the calendar
  LocalTime ->
  -- | Timezone in the calendar
  VTimeZone ->
  -- | Our own timezone
  TimeZone ->
  LocalTime
resolveLocalTimeWithVTimeZone lt vtz hereTZ = utcToLocalTime hereTZ $ resolveLocalTimeWithVTimeZoneToUTC lt vtz

resolveLocalTimeWithVTimeZoneToUTC ::
  -- | Local time in the calendar
  LocalTime ->
  -- | Timezone in the calendar
  VTimeZone ->
  UTCTime
resolveLocalTimeWithVTimeZoneToUTC lt VTimeZone {..} =
  let sel = dateTimeFloating . dtStartDateTimeValue . tzpDTStart
      mostApplicable :: Set TZProp -> Maybe TZProp
      mostApplicable s =
        case takeWhileInclusive ((<= lt) . sel) (sortOn sel (S.toList s)) of
          [] -> Nothing
          (tzprop : _) -> Just tzprop
      mostApplicableStandard = mostApplicable vtzStandardC
      mostApplicableDaylight = mostApplicable vtzDaylightC
      name = tzidValue vtzId
      -- TODO: Only yearly recurrence atm, also support more general recurrence?
      tz = case (mostApplicableStandard, mostApplicableDaylight) of
        (Nothing, Nothing) -> error "TODO: Emit this warning: TimestampNoTimezone"
        (Just tzprop, Nothing) -> makeTZPropTZ name tzprop
        (Nothing, Just tzprop) -> makeTZPropTZ name tzprop
        (Just standard, Just summer) ->
          let yearDay :: LocalTime -> Int
              yearDay = snd . toOrdinalDate . localDay
              yearDaySel :: TZProp -> Int
              yearDaySel = yearDay . sel
              (firstChange, secondChange) =
                if yearDaySel standard <= yearDaySel summer
                  then (standard, summer)
                  else (summer, standard)
           in makeTZPropTZ name $ case (yearDay lt < yearDaySel firstChange, yearDay lt < yearDaySel secondChange) of
                (True, True) -> secondChange
                (True, False) -> secondChange -- Impossible, but fine
                (False, True) -> firstChange
                (False, False) -> secondChange
   in localTimeToUTC tz lt

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive p = go
  where
    go = \case
      [] -> []
      (x : xs) ->
        x
          : if p x
            then go xs
            else []

makeTZPropTZ :: Lazy.Text -> TZProp -> TimeZone
makeTZPropTZ name TZProp {..} =
  TimeZone
    { timeZoneMinutes = utcOffsetValue tzpTZOffsetTo `quot` 60,
      timeZoneSummerOnly = False, -- Not sure if this matters
      timeZoneName = T.unpack $ Lazy.toStrict name
    }

resolveEndDuration :: Timestamp -> Duration -> I TimestampResult
resolveEndDuration = undefined

produceEntry :: Maybe Text -> Maybe Contents -> TimestampsResult -> Entry
produceEntry mt mcts TimestampsResult {..} =
  let h = fromMaybe "No description" $ do
        t <- mt
        header t
   in addTimestampResult "BEGIN" timestampsResultBegin $ addTimestampResult "END" timestampsResultEnd $ addContents mcts $ newEntry h

addContents :: Maybe Contents -> Entry -> Entry
addContents c e = e {entryContents = c}

addTimestampResult :: TimestampName -> TimestampResult -> Entry -> Entry
addTimestampResult tsn TimestampResult {..} =
  maybe id (addTimestamp tsn) timestampResultTimestamp
    . maybe id (addTimestampWarning tsn) timestampResultWarning

addTimestamp :: TimestampName -> Timestamp -> Entry -> Entry
addTimestamp tsn ts e = e {entryTimestamps = M.insert tsn ts (entryTimestamps e)}

addTimestampWarning :: TimestampName -> TimestampWarning -> Entry -> Entry
addTimestampWarning tsn tsw e = e {entryProperties = M.insert pn pv (entryProperties e)}
  where
    pn :: PropertyName
    pn = PropertyName $ timestampNameText tsn <> "_WARNING"
    pv :: PropertyValue
    pv = PropertyValue $ renderTimestampWarning tsw

renderTimestampWarning :: TimestampWarning -> Text
renderTimestampWarning = \case
  TimestampNoTimezone -> "No Timestamp"
  TimestampUnknownTimezone lt -> "Unknown timezone: " <> Lazy.toStrict lt

pruneOldTree :: Tree Entry -> I (Maybe (Tree Entry))
pruneOldTree t = do
  startDay <- asks importEnvStartDay
  let allOldTimestamps e = all ((< startDay) . timestampDay) (entryTimestamps e)
      removeOldTimestamps e = e {entryTimestamps = M.filter ((>= startDay) . timestampDay) (entryTimestamps e)}
      pruneSubtree (Node e es) =
        let es' = mapMaybe pruneSubtree es
         in if allOldTimestamps e && null es'
              then Nothing
              else Just (Node (removeOldTimestamps e) es')
  pure $ pruneSubtree t

toNode :: a -> Tree a
toNode a = Node a []
