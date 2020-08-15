{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import where

import Control.Monad
import Data.Default
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Yaml as Yaml
import Network.HTTP.Client as HTTP (httpLbs, requestFromURI, responseBody)
import Network.HTTP.Client.TLS as HTTP
import Path
import Smos.Calendar.Import.OptParse
import Smos.Calendar.Import.Pick
import Smos.Calendar.Import.Recur
import Smos.Calendar.Import.Render
import Smos.Calendar.Import.Resolve
import Smos.Data
import Smos.Report.Config
import System.Exit
import Text.ICalendar.Parser
import Text.ICalendar.Types as ICal
import YamlParse.Applicative

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
  man <- HTTP.newTlsManager
  forM_ (NE.toList setSources) $ \Source {..} -> do
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
        let errOrCal = parseICalendar def (show uri) $ responseBody resp
        pure errOrCal
      FileOrigin af -> parseICalendarFile def $ fromAbsFile af
    case errOrCal of
      Left err -> die err
      Right (cals, warnings) -> do
        forM_ warnings $ \warning -> putStrLn $ "WARNING: " <> warning
        let conf =
              ProcessConf
                { processConfDebug = setDebug,
                  processConfStart = start,
                  processConfLimit = recurrenceLimit,
                  processConfTimeZone = hereTZ,
                  processConfName = Just originName
                }
        when setDebug $ putStrLn $ unlines ["Using process conf: ", T.unpack (TE.decodeUtf8 (Yaml.encode conf))]
        let sf = processCalendars conf cals
        wd <- resolveDirWorkflowDir setDirectorySettings
        putStrLn $ "Saving to " <> fromRelFile sourceDestinationFile
        let fp = wd </> sourceDestinationFile
        writeSmosFile fp sf

data ProcessConf
  = ProcessConf
      { processConfDebug :: Bool,
        processConfStart :: Day,
        processConfLimit :: Day,
        processConfTimeZone :: TimeZone,
        processConfName :: Maybe String
      }
  deriving (Show, Eq)

instance ToJSON ProcessConf where
  toJSON ProcessConf {..} =
    object $
      concat
        [ ["debug" .= processConfDebug | not processConfDebug],
          ["timezone" .= formatTime defaultTimeLocale timeZoneFormat processConfTimeZone | processConfTimeZone /= utc],
          ["name" .= processConfName | isJust processConfName],
          [ "start" .= formatTime defaultTimeLocale timestampDayFormat processConfStart,
            "limit" .= formatTime defaultTimeLocale timestampDayFormat processConfLimit
          ]
        ]

instance FromJSON ProcessConf where
  parseJSON = viaYamlSchema

instance YamlSchema ProcessConf where
  yamlSchema =
    let dayField = maybeParser (parseTimeM True defaultTimeLocale timestampDayFormat) yamlSchema <?> T.pack timestampDayFormat
        timeZoneField = maybeParser (parseTimeM True defaultTimeLocale timeZoneFormat) yamlSchema <?> T.pack timeZoneFormat
     in objectParser "ProcessConf" $
          ProcessConf
            <$> optionalFieldWithDefault "debug" False "debug mode"
            <*> requiredFieldWith "start" "start day" dayField
            <*> requiredFieldWith "limit" "recurrence limit" dayField
            <*> optionalFieldWithDefaultWith "timezone" utc "time zone" timeZoneField
            <*> optionalField "name" "calendar name"

timeZoneFormat :: String
timeZoneFormat = "%z"

processCalendars :: ProcessConf -> [VCalendar] -> SmosFile
processCalendars ProcessConf {..} cals =
  let recurringEvents = pickEvents cals
      -- TODO just make the limit a local time.
      start = LocalTime processConfStart midnight
      limit = LocalTime processConfLimit midnight
      unresolvedEvents = recurEvents limit recurringEvents
      resolvedEvents = resolveEvents start limit processConfTimeZone unresolvedEvents
   in renderAllEvents resolvedEvents
