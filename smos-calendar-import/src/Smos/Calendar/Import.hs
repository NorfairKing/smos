{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import where

import Autodocodec
import Control.Monad
import Control.Monad.Logger
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.Yaml as Yaml
import qualified ICal
import qualified ICal.Conformance as ICal
import qualified ICal.Recurrence as ICal
import Network.HTTP.Client as HTTP (Manager, httpLbs, requestFromURI, responseBody)
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
import Smos.Directory.Config
import System.Exit
import UnliftIO

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

smosCalendarImport :: IO ()
smosCalendarImport = do
  settings@Settings {..} <- getSettings
  today <- utctDay <$> getCurrentTime
  man <- HTTP.newTlsManager
  results <- runStderrLoggingT $
    filterLogger (\_ ll -> ll >= setLogLevel) $ do
      mapConcurrently (processSource settings today man) setSources
  unless (and results) $ exitWith (ExitFailure 1)

processSource :: Settings -> Day -> HTTP.Manager -> Source -> LoggingT IO Bool
processSource Settings {..} today man Source {..} = do
  let originName = case sourceName of
        Just n -> n
        Nothing -> case sourceOrigin of
          WebOrigin uri -> show uri
          FileOrigin fp -> fromAbsFile fp
  let start = addDays (-7) today
  let recurrenceLimit = addDays 30 today
  mCals <- case sourceOrigin of
    WebOrigin uri -> do
      req <- liftIO $ requestFromURI uri
      logInfoN $
        T.pack $
          concat
            [ unwords
                [ "Fetching",
                  fromMaybe "Unnamed source" sourceName,
                  "from:"
                ],
              "\n",
              show uri
            ]
      resp <- liftIO $ httpLbs req man
      runConformLenientLog $ ICal.parseICalendarByteString $ LB.toStrict $ responseBody resp
    FileOrigin af -> do
      mContents <- liftIO $ forgivingAbsence $ SB.readFile (fromAbsFile af)
      case mContents of
        Nothing -> do
          logErrorN $
            T.pack $
              unwords
                [ "File not found:",
                  fromAbsFile af
                ]
          pure Nothing
        Just cts -> runConformLenientLog $ ICal.parseICalendarByteString cts
  case mCals of
    Nothing -> pure False
    Just cals -> do
      let conf =
            ProcessConf
              { processConfDebug = setDebug,
                processConfStart = start,
                processConfLimit = recurrenceLimit,
                processConfName = Just originName
              }
      logDebugN $
        T.pack $
          unlines
            [ "Using process conf: ",
              T.unpack (TE.decodeUtf8 (Yaml.encode conf))
            ]
      (sf, fine) <- processCalendars conf cals
      wd <- liftIO $ resolveDirWorkflowDir setDirectorySettings
      logInfoN $
        T.pack $
          unwords
            [ "Saving to",
              fromRelFile sourceDestinationFile
            ]
      let fp = wd </> sourceDestinationFile
      liftIO $ writeSmosFile fp sf
      pure fine

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

processCalendars :: ProcessConf -> ICal.ICalendar -> LoggingT IO (SmosFile, Bool)
processCalendars pc@ProcessConf {..} cals = do
  utcEventTups <-
    forM cals $ \cal -> do
      errOrUTCEvents <- runConformLenientLog $ processCalendar pc cal
      case errOrUTCEvents of
        Nothing -> pure (S.empty, False)
        Just utcEvents -> pure (utcEvents, True)
  let fine = all snd utcEventTups
  events <- liftIO $ resolveUTCEvents $ S.unions $ map fst utcEventTups
  let filtered = filterEventsSet processConfStart processConfLimit events
  pure (renderAllEvents filtered, fine)

processCalendar :: ProcessConf -> ICal.Calendar -> ICal.Resolv (Set UTCEvents)
processCalendar ProcessConf {..} cal = do
  let recurringEvents = pickEvents processConfDebug cal
  let timeZones = pickTimeZones cal
  ICal.runR processConfLimit timeZones $ do
    unresolvedEvents <- recurRecurringEvents processConfLimit recurringEvents
    resolveUnresolvedEvents unresolvedEvents

runConformLenientLog :: (Exception ue, Exception fe, Exception w) => ICal.Conform ue fe w a -> LoggingT IO (Maybe a)
runConformLenientLog func = case ICal.runConformLenient func of
  Left err -> do
    logErrorN $
      T.pack $
        unlines
          [ "Could not import ICal because of this error",
            displayException err
          ]
    pure Nothing
  Right (a, ICal.Notes {..}) -> do
    forM_ notesFixableErrors $ \fe -> do
      logWarnN $
        T.pack $
          unlines
            [ "ICal was not conformant, but we guessed something",
              displayException fe
            ]
    forM_ notesWarnings $ \w -> do
      logWarnN $
        T.pack $
          unlines
            [ "ICal imported with warnings",
              displayException w
            ]
    pure (Just a)
