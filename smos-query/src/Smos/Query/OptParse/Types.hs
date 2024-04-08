{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.OptParse.Types
  ( module Smos.Report.Clock.Types,
    module Smos.Report.Agenda.Types,
    module Smos.Query.OptParse.Types,
    module Smos.Directory.ShouldPrint,
  )
where

import Autodocodec
import Data.Aeson (ToJSON (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Smos.CLI.Colour
import Smos.CLI.OptParse
import Smos.Data
import Smos.Directory.Archive
import Smos.Directory.OptParse.Types
import Smos.Directory.ShouldPrint
import Smos.Report.Agenda.Types
import Smos.Report.Clock.Types
import Smos.Report.Filter
import Smos.Report.OptParse.Types (ContextName)
import qualified Smos.Report.OptParse.Types as Report
import Smos.Report.Period
import Smos.Report.Projection
import Smos.Report.Report
import Smos.Report.Sorter
import Smos.Report.Time
import Smos.Report.TimeBlock

data Arguments
  = Arguments Command (FlagsWithConfigFile Flags)

data Instructions
  = Instructions Dispatch Settings

data Command
  = CommandEntry !EntryFlags
  | CommandPreparedReport !PreparedReportFlags
  | CommandWaiting !WaitingFlags
  | CommandNext !NextFlags
  | CommandOngoing !OngoingFlags
  | CommandClock !ClockFlags
  | CommandAgenda !AgendaFlags
  | CommandProjects !ProjectsFlags
  | CommandStuck !StuckFlags
  | CommandWork !WorkFlags
  | CommandFree !FreeFlags
  | CommandLog !LogFlags
  | CommandStats !StatsFlags
  | CommandTags !TagsFlags

data EntryFlags = EntryFlags
  { entryFlagFilter :: !(Maybe EntryFilter),
    entryFlagProjection :: !(Maybe (NonEmpty Projection)),
    entryFlagSorter :: !(Maybe Sorter),
    entryFlagHideArchive :: !(Maybe HideArchive),
    entryFlagOutputFormat :: !(Maybe OutputFormat)
  }

data PreparedReportFlags = PreparedReportFlags
  { preparedReportFlagReportName :: !(Maybe Text),
    preparedReportFlagOutputFormat :: !(Maybe OutputFormat)
  }

data WaitingFlags = WaitingFlags
  { waitingFlagFilter :: !(Maybe EntryFilter),
    waitingFlagHideArchive :: !(Maybe HideArchive),
    waitingFlagThreshold :: !(Maybe Time)
  }

data NextFlags = NextFlags
  { nextFlagFilter :: !(Maybe EntryFilter),
    nextFlagHideArchive :: !(Maybe HideArchive)
  }

data OngoingFlags = OngoingFlags
  { ongoingFlagFilter :: !(Maybe EntryFilter),
    ongoingFlagHideArchive :: !(Maybe HideArchive)
  }

data ClockFlags = ClockFlags
  { clockFlagFilter :: !(Maybe EntryFilter),
    clockFlagPeriodFlags :: !(Maybe Period),
    clockFlagBlockFlags :: !(Maybe TimeBlock),
    clockFlagOutputFormat :: !(Maybe OutputFormat),
    clockFlagClockFormat :: !(Maybe ClockFormatFlags),
    clockFlagReportStyle :: !(Maybe ClockReportStyle),
    clockFlagHideArchive :: !(Maybe HideArchive)
  }

data ClockFormatFlags
  = ClockFormatTemporalFlag !(Maybe TemporalClockResolution)
  | ClockFormatDecimalFlag !(Maybe DecimalClockResolution)

data AgendaFlags = AgendaFlags
  { agendaFlagFilter :: !(Maybe EntryFilter),
    agendaFlagHistoricity :: !(Maybe AgendaHistoricity),
    agendaFlagBlock :: !(Maybe TimeBlock),
    agendaFlagHideArchive :: !(Maybe HideArchive),
    agendaFlagPeriod :: !(Maybe Period)
  }

data ProjectsFlags = ProjectsFlags
  { projectsFlagFilter :: !(Maybe ProjectFilter)
  }

data StuckFlags = StuckFlags
  { stuckFlagFilter :: !(Maybe ProjectFilter),
    stuckFlagThreshold :: !(Maybe Time)
  }

data WorkFlags = WorkFlags
  { workFlagContext :: !(Maybe ContextName),
    workFlagTime :: !(Maybe Time),
    workFlagFilter :: !(Maybe EntryFilter),
    workFlagProjection :: !(Maybe (NonEmpty Projection)),
    workFlagSorter :: !(Maybe Sorter),
    workFlagHideArchive :: !(Maybe HideArchive),
    workFlagWaitingThreshold :: !(Maybe Time),
    workFlagStuckThreshold :: !(Maybe Time)
  }

data FreeFlags = FreeFlags
  { freeFlagPeriodFlags :: !(Maybe Period),
    freeFlagMinimumTime :: !(Maybe Time),
    freeFlagHideArchive :: !(Maybe HideArchive)
  }

data LogFlags = LogFlags
  { logFlagFilter :: !(Maybe EntryFilter),
    logFlagPeriodFlags :: !(Maybe Period),
    logFlagBlockFlags :: !(Maybe TimeBlock),
    logFlagHideArchive :: !(Maybe HideArchive)
  }

newtype StatsFlags = StatsFlags
  { statsFlagPeriodFlags :: Maybe Period
  }

data TagsFlags = TagsFlags
  { tagsFlagFilter :: !(Maybe EntryFilter),
    tagsFlagHideArchive :: !(Maybe HideArchive)
  }

newtype Flags = Flags
  { flagReportFlags :: Report.Flags
  }

data Environment = Environment
  { envReportEnvironment :: !Report.Environment,
    envHideArchive :: !(Maybe HideArchive)
  }

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    { confReportConf = Report.defaultConfiguration,
      confHideArchive = Nothing,
      confPreparedReportConfiguration = Nothing,
      confColourConfiguration = Nothing
    }

data Configuration = Configuration
  { confReportConf :: !Report.Configuration,
    confHideArchive :: !(Maybe HideArchive),
    confPreparedReportConfiguration :: !(Maybe PreparedReportConfiguration),
    confColourConfiguration :: !(Maybe ColourConfiguration)
  }
  deriving (ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> objectCodec
          .= confReportConf
        <*> optionalFieldOrNull "hide-archive" "Whether or not to consider the archive, by default"
          .= confHideArchive
        <*> optionalFieldOrNull preparedReportConfigurationKey "Prepared report config"
          .= confPreparedReportConfiguration
        <*> colourConfigurationTopLevelObjectCodec
          .= confColourConfiguration

preparedReportConfigurationKey :: Text
preparedReportConfigurationKey = "report"

data PreparedReportConfiguration = PreparedReportConfiguration
  { preparedReportConfAvailableReports :: !(Maybe (Map Text PreparedReport))
  }

instance HasCodec PreparedReportConfiguration where
  codec =
    object "PreparedReportConfiguration" $
      PreparedReportConfiguration <$> optionalFieldOrNull "reports" "Custom reports" .= preparedReportConfAvailableReports

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

data EntrySettings = EntrySettings
  { entrySetFilter :: !(Maybe EntryFilter),
    entrySetProjection :: !(NonEmpty Projection),
    entrySetSorter :: !(Maybe Sorter),
    entrySetHideArchive :: !HideArchive,
    entrySetOutputFormat :: !OutputFormat
  }

data PreparedReportSettings = PreparedReportSettings
  { preparedReportSetReportName :: !(Maybe Text),
    preparedReportSetAvailableReports :: !(Map Text PreparedReport),
    preparedReportSetOutputFormat :: !OutputFormat
  }

data WaitingSettings = WaitingSettings
  { waitingSetFilter :: !(Maybe EntryFilter),
    waitingSetHideArchive :: !HideArchive,
    waitingSetThreshold :: !Time
  }

data NextSettings = NextSettings
  { nextSetFilter :: !(Maybe EntryFilter),
    nextSetHideArchive :: !HideArchive
  }

data OngoingSettings = OngoingSettings
  { ongoingSetFilter :: !(Maybe EntryFilter),
    ongoingSetHideArchive :: !HideArchive
  }

data ClockSettings = ClockSettings
  { clockSetFilter :: !(Maybe EntryFilter),
    clockSetPeriod :: !Period,
    clockSetBlock :: !TimeBlock,
    clockSetOutputFormat :: !OutputFormat,
    clockSetClockFormat :: !ClockFormat,
    clockSetReportStyle :: !ClockReportStyle,
    clockSetHideArchive :: !HideArchive
  }

data AgendaSettings = AgendaSettings
  { agendaSetFilter :: !(Maybe EntryFilter),
    agendaSetHistoricity :: !AgendaHistoricity,
    agendaSetBlock :: !TimeBlock,
    agendaSetHideArchive :: !HideArchive,
    agendaSetPeriod :: !Period
  }

data ProjectsSettings = ProjectsSettings
  { projectsSetFilter :: !(Maybe ProjectFilter)
  }

data StuckSettings = StuckSettings
  { stuckSetFilter :: !(Maybe ProjectFilter),
    stuckSetThreshold :: !Time
  }

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

data FreeSettings = FreeSettings
  { freeSetPeriod :: !Period,
    freeSetMinimumTime :: !(Maybe Time),
    freeSetHideArchive :: !HideArchive,
    freeSetEarliestTimeOfDay :: !(Maybe TimeOfDay),
    freeSetLatestTimeOfDay :: !(Maybe TimeOfDay)
  }

data LogSettings = LogSettings
  { logSetFilter :: !(Maybe EntryFilter),
    logSetPeriod :: !Period,
    logSetBlock :: !TimeBlock,
    logSetHideArchive :: !HideArchive
  }

data StatsSettings = StatsSettings
  { statsSetPeriod :: !Period
  }

data TagsSettings = TagsSettings
  { tagsSetFilter :: !(Maybe EntryFilter),
    tagsSetHideArchive :: !HideArchive
  }

data OutputFormat
  = OutputPretty
  | OutputYaml
  | OutputJSON
  | OutputJSONPretty

data Settings = Settings
  { settingDirectorySettings :: !DirectorySettings,
    settingColourSettings :: !ColourSettings
  }
