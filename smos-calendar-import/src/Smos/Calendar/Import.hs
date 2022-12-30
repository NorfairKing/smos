{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import where

import Autodocodec
import Control.Arrow (left)
import Control.Concurrent.Async
import Control.Exception (displayException)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.Yaml as Yaml
import qualified ICal
import qualified ICal.Conformance as ICal
import qualified ICal.Recurrence as ICal
import Network.HTTP.Client as HTTP (httpLbs, requestFromURI, responseBody)
import Network.HTTP.Client.TLS as HTTP
import Path
import Path.IO
import Smos.Calendar.Import.Filter
import Smos.Calendar.Import.OptParse
import Smos.Calendar.Import.Pick
import Smos.Calendar.Import.Recur
import Smos.Calendar.Import.Render
import Smos.Calendar.Import.ResolveLocal
import Smos.Calendar.Import.ResolveZones
import Smos.Calendar.Import.UTCEvent
import Smos.Data
import Smos.Report.Config
import System.Exit

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
  man <- HTTP.newTlsManager
  results <- forConcurrently setSources $ \Source {..} -> do
    let originName = case sourceName of
          Just n -> n
          Nothing -> case sourceOrigin of
            WebOrigin uri -> show uri
            FileOrigin fp -> fromAbsFile fp
    errOrCal <- case sourceOrigin of
      WebOrigin uri -> do
        req <- requestFromURI uri
        putStrLn $ "Fetching: " <> show uri
        resp <- httpLbs req man
        pure $ left displayException $ ICal.runConform $ ICal.parseICalendarByteString $ LB.toStrict $ responseBody resp
      FileOrigin af -> do
        mContents <- forgivingAbsence $ SB.readFile (fromAbsFile af)
        pure $ case mContents of
          Nothing -> Left $ unwords ["File not found:", fromAbsFile af]
          Just cts -> left displayException $ ICal.runConform $ ICal.parseICalendarByteString cts
    case errOrCal of
      Left err -> do
        putStrLn $
          unlines
            [ unwords ["Error while parsing calendar from source:", originName],
              err
            ]
        pure False
      Right (cals, warnings) -> do
        forM_ warnings $ \warning -> putStrLn $ "WARNING: " <> displayException warning
        let conf =
              ProcessConf
                { processConfDebug = setDebug,
                  processConfStart = start,
                  processConfLimit = recurrenceLimit,
                  processConfName = Just originName
                }
        when setDebug $ putStrLn $ unlines ["Using process conf: ", T.unpack (TE.decodeUtf8 (Yaml.encode conf))]
        sf <- processCalendars conf cals
        wd <- resolveDirWorkflowDir setDirectorySettings
        putStrLn $ "Saving to " <> fromRelFile sourceDestinationFile
        let fp = wd </> sourceDestinationFile
        writeSmosFile fp sf
        pure True
  unless (and results) $ exitWith (ExitFailure 1)

data ProcessConf = ProcessConf
  { processConfDebug :: Bool,
    processConfStart :: Day,
    processConfLimit :: Day,
    processConfName :: Maybe String
  }
  deriving stock (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec ProcessConf)

instance HasCodec ProcessConf where
  codec =
    object "ProcessConf" $
      ProcessConf
        <$> optionalFieldWithDefault "debug" False "debug mode" .= processConfDebug
        <*> requiredFieldWith "start" dayCodec "start day" .= processConfStart
        <*> requiredFieldWith "limit" dayCodec "recurrence limit" .= processConfLimit
        <*> optionalField "name" "calendar name" .= processConfName

processCalendars :: ProcessConf -> ICal.ICalendar -> IO SmosFile
processCalendars pc@ProcessConf {..} cals = do
  utcEvents <- fmap S.unions $
    forM cals $ \cal ->
      case ICal.runConformLenient $ processCalendar pc cal of
        Left err -> do
          putStrLn $ "WARNING: Could not import calendar but continueing anyway" <> displayException err
          pure S.empty
        Right (utcEvents, ICal.Notes {..}) -> do
          forM_ notesFixableErrors $ \fe -> do
            putStrLn $ "WARNING: ICal was not conformant, but we guessed something" <> displayException fe
          forM_ notesWarnings $ \w -> do
            putStrLn $ "WARNING: ICal was conformant, but did not adhere to some SHOULD, or SHOULD NOT's" <> displayException w
          pure utcEvents
  events <- resolveUTCEvents utcEvents
  let filtered = filterEventsSet processConfStart processConfLimit events
  pure $ renderAllEvents filtered

processCalendar :: ProcessConf -> ICal.Calendar -> ICal.Resolv (Set UTCEvents)
processCalendar ProcessConf {..} cal = do
  let recurringEvents = pickEvents processConfDebug cal
  let timeZones = pickTimeZones cal
  ICal.runR processConfLimit timeZones $ do
    unresolvedEvents <- recurRecurringEvents processConfLimit recurringEvents
    resolveUnresolvedEvents unresolvedEvents
