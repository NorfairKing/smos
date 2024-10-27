{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Query.OptParse
  ( module Smos.Query.OptParse,
    module Smos.Report.Clock.Types,
    module Smos.Report.Agenda.Types,
    module Smos.Directory.ShouldPrint,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import OptEnvConf
import Paths_smos_query (version)
import Smos.CLI.Colour
import Smos.CLI.OptParse
import Smos.Data
import Smos.Directory.Archive
import Smos.Directory.OptParse
import Smos.Directory.ShouldPrint
import Smos.Report.Agenda.Types
import Smos.Report.Clock.Types
import Smos.Report.Filter
import Smos.Report.OptParse (ContextName)
import qualified Smos.Report.OptParse as Report
import Smos.Report.Period
import Smos.Report.Projection
import Smos.Report.Report
import Smos.Report.Sorter
import Smos.Report.Time
import Smos.Report.TimeBlock

getInstructions :: IO Instructions
getInstructions =
  runSettingsParser version $
    unlines $
      concat
        [ [ "Smos' query tool",
            ""
          ],
          readDataVersionsHelpMessage
        ]

parseWaitingThresholdOption :: Parser Time
parseWaitingThresholdOption =
  setting
    [ help "The threshold at which to color waiting entries red",
      option,
      reader $ eitherReader $ parseTime . T.pack,
      name "threshold",
      metavar "TIME",
      value Report.defaultWaitingThreshold
    ]

parseStuckThresholdOption :: Parser Time
parseStuckThresholdOption =
  setting
    [ help "The threshold at which to color stuck projects red",
      option,
      reader $ eitherReader $ parseTime . T.pack,
      name "threshold",
      metavar "TIME",
      value Report.defaultStuckThreshold
    ]

data Instructions
  = Instructions Dispatch Settings

instance HasParser Instructions where
  settingsParser =
    withSmosConfig $
      Instructions
        <$> settingsParser
        <*> settingsParser

data Dispatch
  = DispatchEntry !EntrySettings
  | DispatchPreparedReport !PreparedReportSettings
  | DispatchWaiting !WaitingSettings
  | DispatchNext !NextSettings
  | DispatchOngoing !OngoingSettings
  | DispatchClock !ClockSettings
  | DispatchAgenda !AgendaSettings
  | DispatchProjects !ProjectsSettings
  | DispatchStuck !StuckSettings
  | DispatchWork !WorkSettings
  | DispatchFree !FreeSettings
  | DispatchLog !LogSettings
  | DispatchStats !StatsSettings
  | DispatchTags !TagsSettings

instance HasParser Dispatch where
  settingsParser =
    commands
      [ command "entry" "Run a custom report with given filter, sorter, ..." $ DispatchEntry <$> settingsParser,
        command "report" "Run a prepared report" $ DispatchPreparedReport <$> settingsParser,
        command "waiting" "Run the waiting report" $ DispatchWaiting <$> settingsParser,
        command "next" "Run the next actions report" $ DispatchNext <$> settingsParser,
        command "ongoing" "Show ongoing entries" $ DispatchOngoing <$> settingsParser,
        command "clock" "Run the clock report" $ DispatchClock <$> settingsParser,
        command "agenda" "Show timestamps in an agenda" $ DispatchAgenda <$> settingsParser,
        command "projects" "Run the projects overview" $ DispatchProjects <$> settingsParser,
        command "stuck" "Run the stuck projects report" $ DispatchStuck <$> settingsParser,
        command "work" "Run the work report" $ DispatchWork <$> settingsParser,
        command "free" "Find a free slot for a meeting" $ DispatchFree <$> settingsParser,
        command "log" "Show a log of what has happened" $ DispatchLog <$> settingsParser,
        command "stats" "Show statitistics about entries being changed" $ DispatchStats <$> settingsParser,
        command "tags" "List all the tags that are in use" $ DispatchTags <$> settingsParser
      ]

data EntrySettings = EntrySettings
  { entrySetFilter :: !(Maybe EntryFilter),
    entrySetProjection :: !(NonEmpty Projection),
    entrySetSorter :: !(Maybe Sorter),
    entrySetHideArchive :: !HideArchive,
    entrySetOutputFormat :: !OutputFormat
  }

instance HasParser EntrySettings where
  settingsParser = parseEntrySettings

{-# ANN parseEntrySettings ("NOCOVER" :: String) #-}
parseEntrySettings :: OptEnvConf.Parser EntrySettings
parseEntrySettings = subEnv_ "entry" $
  subConfig_ "entry" $ do
    entrySetFilter <- optional Report.parseFilterArgs
    entrySetProjection <- Report.parseProjectionOptions
    entrySetSorter <- optional Report.parseSorterOptions
    entrySetHideArchive <- withDefault HideArchive settingsParser
    entrySetOutputFormat <- settingsParser
    pure EntrySettings {..}

data PreparedReportSettings = PreparedReportSettings
  { preparedReportSetReportName :: !(Maybe Text),
    preparedReportSetAvailableReports :: !(Map Text PreparedReport),
    preparedReportSetOutputFormat :: !OutputFormat
  }

instance HasParser PreparedReportSettings where
  settingsParser = parsePreparedReportSettings

{-# ANN parsePreparedReportSettings ("NOCOVER" :: String) #-}
parsePreparedReportSettings :: OptEnvConf.Parser PreparedReportSettings
parsePreparedReportSettings = subEnv_ "report" $
  subConfig_ "report" $ do
    preparedReportSetReportName <-
      optional $
        setting
          [ help "name of the report",
            argument,
            reader str,
            metavar "NAME"
          ]
    preparedReportSetAvailableReports <-
      setting
        [ help "available reports",
          conf "reports",
          value M.empty
        ]
    preparedReportSetOutputFormat <- settingsParser
    pure PreparedReportSettings {..}

data WaitingSettings = WaitingSettings
  { waitingSetFilter :: !(Maybe EntryFilter),
    waitingSetHideArchive :: !HideArchive,
    waitingSetThreshold :: !Time
  }

instance HasParser WaitingSettings where
  settingsParser = parseWaitingSettings

{-# ANN parseWaitingSettings ("NOCOVER" :: String) #-}
parseWaitingSettings :: OptEnvConf.Parser WaitingSettings
parseWaitingSettings = subEnv_ "waiting" $
  subConfig_ "waiting" $ do
    waitingSetFilter <- optional Report.parseFilterArgs
    waitingSetHideArchive <- withDefault HideArchive settingsParser
    waitingSetThreshold <- parseWaitingThresholdOption
    pure WaitingSettings {..}

data NextSettings = NextSettings
  { nextSetFilter :: !(Maybe EntryFilter),
    nextSetHideArchive :: !HideArchive
  }

instance HasParser NextSettings where
  settingsParser = parseNextSettings

{-# ANN parseNextSettings ("NOCOVER" :: String) #-}
parseNextSettings :: OptEnvConf.Parser NextSettings
parseNextSettings = subEnv_ "next" $
  subConfig_ "next" $ do
    nextSetFilter <- optional Report.parseFilterArgs
    nextSetHideArchive <- withDefault HideArchive settingsParser
    pure NextSettings {..}

data OngoingSettings = OngoingSettings
  { ongoingSetFilter :: !(Maybe EntryFilter),
    ongoingSetHideArchive :: !HideArchive
  }

instance HasParser OngoingSettings where
  settingsParser = parseOngoingSettings

{-# ANN parseOngoingSettings ("NOCOVER" :: String) #-}
parseOngoingSettings :: OptEnvConf.Parser OngoingSettings
parseOngoingSettings = subEnv_ "ongoing" $
  subConfig_ "ongoing" $ do
    ongoingSetFilter <- optional Report.parseFilterArgs
    ongoingSetHideArchive <- withDefault HideArchive settingsParser
    pure OngoingSettings {..}

data ClockSettings = ClockSettings
  { clockSetFilter :: !(Maybe EntryFilter),
    clockSetPeriod :: !Period,
    clockSetBlock :: !TimeBlock,
    clockSetOutputFormat :: !OutputFormat,
    clockSetClockFormat :: !ClockFormat,
    clockSetReportStyle :: !ClockReportStyle,
    clockSetHideArchive :: !HideArchive
  }

instance HasParser ClockSettings where
  settingsParser = parseClockSettings

{-# ANN parseClockSettings ("NOCOVER" :: String) #-}
parseClockSettings :: OptEnvConf.Parser ClockSettings
parseClockSettings = subEnv_ "clock" $
  subConfig_ "clock" $ do
    clockSetFilter <- optional Report.parseFilterArgs
    clockSetPeriod <- withDefault AllTime settingsParser
    clockSetBlock <- withDefault OneBlock settingsParser
    clockSetOutputFormat <- settingsParser
    clockSetClockFormat <- settingsParser
    clockSetReportStyle <- settingsParser
    clockSetHideArchive <- withDefault Don'tHideArchive settingsParser
    pure ClockSettings {..}

data AgendaSettings = AgendaSettings
  { agendaSetFilter :: !(Maybe EntryFilter),
    agendaSetHistoricity :: !AgendaHistoricity,
    agendaSetBlock :: !TimeBlock,
    agendaSetHideArchive :: !HideArchive,
    agendaSetPeriod :: !Period
  }

instance HasParser AgendaSettings where
  settingsParser = parseAgendaSettings

{-# ANN parseAgendaSettings ("NOCOVER" :: String) #-}
parseAgendaSettings :: OptEnvConf.Parser AgendaSettings
parseAgendaSettings =
  subEnv_ "agenda" $
    subConfig_ "agenda" $
      ( do
          agendaSetFilter <- optional Report.parseFilterArgs
          agendaSetHistoricity <- settingsParser
          agendaSetHideArchive <- withDefault HideArchive settingsParser
          pure $ \(agendaSetPeriod, agendaSetBlock) ->
            AgendaSettings {..}
      )
        <*> fmap
          ( \(period, mBlock) ->
              -- See Note [Agenda command defaults]
              let defaultBlock = case period of
                    AllTime -> OneBlock
                    LastYear -> MonthBlock
                    ThisYear -> MonthBlock
                    NextYear -> MonthBlock
                    LastMonth -> WeekBlock
                    ThisMonth -> WeekBlock
                    NextMonth -> WeekBlock
                    LastWeek -> DayBlock
                    ThisWeek -> DayBlock
                    NextWeek -> DayBlock
                    _ -> OneBlock
               in (period, fromMaybe defaultBlock mBlock)
          )
          ( (,)
              -- Note [Agenda command defaults]
              -- The default here is 'AllTime' for good reason.
              --
              -- You may think that 'Today' is a better default because smos-calendar-import fills up
              -- your agenda too much for it to be useful.
              --
              -- However, as a beginner you want to be able to run smos-query agenda to see your
              -- SCHEDULED and DEADLINE timestamps in the near future.
              -- By the time users figure out how to use smos-calendar-import, they will probably
              -- either already use "smos-query work" or have an alias for 'smos-query agenda --today'
              -- if they need it.
              <$> withShownDefault AllTime "all" settingsParser
              <*> optional settingsParser
          )

data ProjectsSettings = ProjectsSettings
  { projectsSetFilter :: !(Maybe ProjectFilter)
  }

instance HasParser ProjectsSettings where
  settingsParser = parseProjectsSettings

{-# ANN parseProjectsSettings ("NOCOVER" :: String) #-}
parseProjectsSettings :: OptEnvConf.Parser ProjectsSettings
parseProjectsSettings = subEnv_ "projects" $
  subConfig_ "projects" $ do
    projectsSetFilter <- Report.parseProjectFilterArgs
    pure ProjectsSettings {..}

data StuckSettings = StuckSettings
  { stuckSetFilter :: !(Maybe ProjectFilter),
    stuckSetThreshold :: !Time
  }

instance HasParser StuckSettings where
  settingsParser = parseStuckSettings

{-# ANN parseStuckSettings ("NOCOVER" :: String) #-}
parseStuckSettings :: OptEnvConf.Parser StuckSettings
parseStuckSettings = subEnv_ "stuck" $
  subConfig_ "stuck" $ do
    stuckSetFilter <- Report.parseProjectFilterArgs
    stuckSetThreshold <- parseStuckThresholdOption
    pure StuckSettings {..}

data WorkSettings = WorkSettings
  { workSetContext :: !(Maybe ContextName),
    workSetContexts :: !(Map ContextName EntryFilter),
    workSetChecks :: !(Set EntryFilter),
    workSetTime :: !(Maybe Time),
    workSetTimeProperty :: !(Maybe PropertyName),
    workSetBaseFilter :: !(Maybe EntryFilter),
    workSetFilter :: !(Maybe EntryFilter),
    workSetProjection :: !(NonEmpty Projection),
    workSetSorter :: !(Maybe Sorter),
    workSetHideArchive :: !HideArchive,
    workSetWaitingThreshold :: !Time,
    workSetStuckThreshold :: !Time
  }

instance HasParser WorkSettings where
  settingsParser = parseWorkSettings

{-# ANN parseWorkSettings ("NOCOVER" :: String) #-}
parseWorkSettings :: OptEnvConf.Parser WorkSettings
parseWorkSettings = do
  let sub = subEnv_ "work" . subConfig_ "work"
  workSetContext <- sub $ optional settingsParser
  workSetContexts <- sub Report.parseWorkContexts
  workSetChecks <- sub Report.parseWorkChecks
  workSetTime <- sub $ optional settingsParser
  workSetTimeProperty <- sub Report.parseWorkTimeProperty
  workSetBaseFilter <- sub Report.parseWorkBaseFilter
  workSetFilter <- sub $ optional Report.parseFilterOptions
  workSetProjection <- sub Report.parseProjectionOptions
  workSetSorter <- sub $ optional Report.parseSorterOptions
  workSetHideArchive <- sub $ withDefault HideArchive settingsParser
  workSetWaitingThreshold <- subAll "waiting" parseWaitingThresholdOption
  workSetStuckThreshold <- subAll "stuck" parseStuckThresholdOption
  pure WorkSettings {..}

data FreeSettings = FreeSettings
  { freeSetPeriod :: !Period,
    freeSetMinimumTime :: !(Maybe Time),
    freeSetHideArchive :: !HideArchive,
    freeSetEarliestTimeOfDay :: !(Maybe TimeOfDay),
    freeSetLatestTimeOfDay :: !(Maybe TimeOfDay)
  }

instance HasParser FreeSettings where
  settingsParser = parseFreeSettings

{-# ANN parseFreeSettings ("NOCOVER" :: String) #-}
parseFreeSettings :: OptEnvConf.Parser FreeSettings
parseFreeSettings = subEnv_ "free" $
  subConfig_ "free" $ do
    freeSetPeriod <- withDefault ComingWeek settingsParser
    freeSetMinimumTime <-
      optional $
        setting
          [ help "Minimum amount of free time to show a free time slot",
            argument,
            reader $ eitherReader $ parseTime . T.pack,
            metavar "TIME"
          ]
    freeSetHideArchive <- withDefault HideArchive settingsParser
    freeSetEarliestTimeOfDay <-
      optional $
        setting
          [ help "Earliest time of day",
            option,
            reader $ maybeReader $ parseTimeM True defaultTimeLocale "%H:%M",
            reader auto,
            long "earliest",
            metavar "TIME_OF_DAY"
          ]
    freeSetLatestTimeOfDay <-
      optional $
        setting
          [ help "Latest time of day",
            option,
            reader $ maybeReader $ parseTimeM True defaultTimeLocale "%H:%M",
            reader auto,
            long "latest",
            metavar "TIME_OF_DAY"
          ]
    pure FreeSettings {..}

data LogSettings = LogSettings
  { logSetFilter :: !(Maybe EntryFilter),
    logSetPeriod :: !Period,
    logSetBlock :: !TimeBlock,
    logSetHideArchive :: !HideArchive
  }

instance HasParser LogSettings where
  settingsParser = parseLogSettings

{-# ANN parseLogSettings ("NOCOVER" :: String) #-}
parseLogSettings :: OptEnvConf.Parser LogSettings
parseLogSettings = subEnv_ "log" $
  subConfig_ "log" $ do
    logSetFilter <- optional Report.parseFilterArgs
    logSetPeriod <- withDefault Today settingsParser
    logSetBlock <- withDefault DayBlock settingsParser
    logSetHideArchive <- withDefault Don'tHideArchive settingsParser
    pure LogSettings {..}

data StatsSettings = StatsSettings
  { statsSetPeriod :: !Period
  }

instance HasParser StatsSettings where
  settingsParser = parseStatsSettings

{-# ANN parseStatsSettings ("NOCOVER" :: String) #-}
parseStatsSettings :: OptEnvConf.Parser StatsSettings
parseStatsSettings = subEnv_ "stats" $
  subConfig_ "stats" $ do
    statsSetPeriod <- withDefault AllTime settingsParser
    pure StatsSettings {..}

data TagsSettings = TagsSettings
  { tagsSetFilter :: !(Maybe EntryFilter),
    tagsSetHideArchive :: !HideArchive
  }

instance HasParser TagsSettings where
  settingsParser = parseTagsSettings

{-# ANN parseTagsSettings ("NOCOVER" :: String) #-}
parseTagsSettings :: OptEnvConf.Parser TagsSettings
parseTagsSettings = subEnv_ "tags" $
  subConfig_ "tags" $ do
    tagsSetFilter <- optional Report.parseFilterArgs
    tagsSetHideArchive <- withDefault HideArchive settingsParser
    pure TagsSettings {..}

data OutputFormat
  = OutputPretty
  | OutputYaml
  | OutputJSON
  | OutputJSONPretty

instance HasParser OutputFormat where
  settingsParser =
    withShownDefault OutputPretty "pretty" $
      choice
        [ setting
            [ help "pretty text",
              switch OutputPretty,
              long "pretty"
            ],
          setting
            [ help "Yaml",
              switch OutputYaml,
              long "yaml"
            ],
          setting
            [ help "single-line JSON",
              switch OutputJSON,
              long "json"
            ],
          setting
            [ help "pretty JSON",
              switch OutputJSONPretty,
              long "pretty-json"
            ]
        ]

data Settings = Settings
  { settingDirectorySettings :: !DirectorySettings,
    settingColourSettings :: !ColourSettings
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = do
  settingDirectorySettings <- settingsParser
  settingColourSettings <- settingsParser
  pure Settings {..}
