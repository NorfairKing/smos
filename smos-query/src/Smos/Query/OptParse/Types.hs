{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.OptParse.Types
  ( module Smos.Report.Clock.Types,
    module Smos.Report.Agenda.Types,
    module Smos.Query.OptParse.Types,
    module Smos.Report.ShouldPrint,
  )
where

import Autodocodec
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Smos.CLI.Colour
import Smos.CLI.OptParse
import Smos.Data
import Smos.Report.Agenda.Types
import Smos.Report.Archive
import Smos.Report.Clock.Types
import Smos.Report.Config
import Smos.Report.Filter
import qualified Smos.Report.OptParse.Types as Report
import Smos.Report.Period
import Smos.Report.Projection
import Smos.Report.Report
import Smos.Report.ShouldPrint
import Smos.Report.Sorter
import Smos.Report.Time
import Smos.Report.TimeBlock

data Arguments
  = Arguments Command (FlagsWithConfigFile Flags)
  deriving (Show, Eq)

data Instructions
  = Instructions Dispatch Settings

data Command
  = CommandEntry !EntryFlags
  | CommandReport !ReportFlags
  | CommandWaiting !WaitingFlags
  | CommandNext !NextFlags
  | CommandClock !ClockFlags
  | CommandAgenda !AgendaFlags
  | CommandProjects !ProjectsFlags
  | CommandStuck !StuckFlags
  | CommandWork !WorkFlags
  | CommandLog !LogFlags
  | CommandStats !StatsFlags
  | CommandTags !TagsFlags
  deriving (Show, Eq)

data EntryFlags = EntryFlags
  { entryFlagFilter :: !(Maybe EntryFilter),
    entryFlagProjection :: !(Maybe (NonEmpty Projection)),
    entryFlagSorter :: !(Maybe Sorter),
    entryFlagHideArchive :: !(Maybe HideArchive),
    entryFlagOutputFormat :: !(Maybe OutputFormat)
  }
  deriving (Show, Eq)

data ReportFlags = ReportFlags
  { reportFlagReportName :: !(Maybe Text),
    reportFlagOutputFormat :: !(Maybe OutputFormat)
  }
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data WaitingFlags = WaitingFlags
  { waitingFlagFilter :: !(Maybe EntryFilter),
    waitingFlagHideArchive :: !(Maybe HideArchive),
    waitingFlagThreshold :: !(Maybe Time)
  }
  deriving (Show, Eq)

data NextFlags = NextFlags
  { nextFlagFilter :: !(Maybe EntryFilter),
    nextFlagHideArchive :: !(Maybe HideArchive)
  }
  deriving (Show, Eq)

data ClockFlags = ClockFlags
  { clockFlagFilter :: !(Maybe EntryFilter),
    clockFlagPeriodFlags :: !(Maybe Period),
    clockFlagBlockFlags :: !(Maybe TimeBlock),
    clockFlagOutputFormat :: !(Maybe OutputFormat),
    clockFlagClockFormat :: !(Maybe ClockFormatFlags),
    clockFlagReportStyle :: !(Maybe ClockReportStyle),
    clockFlagHideArchive :: !(Maybe HideArchive)
  }
  deriving (Show, Eq)

data ClockFormatFlags
  = ClockFormatTemporalFlag !(Maybe TemporalClockResolution)
  | ClockFormatDecimalFlag !(Maybe DecimalClockResolution)
  deriving (Show, Eq)

data AgendaFlags = AgendaFlags
  { agendaFlagFilter :: !(Maybe EntryFilter),
    agendaFlagHistoricity :: !(Maybe AgendaHistoricity),
    agendaFlagBlock :: !(Maybe TimeBlock),
    agendaFlagHideArchive :: !(Maybe HideArchive),
    agendaFlagPeriod :: !(Maybe Period)
  }
  deriving (Show, Eq)

data ProjectsFlags = ProjectsFlags
  { projectsFlagFilter :: !(Maybe ProjectFilter)
  }
  deriving (Show, Eq)

data StuckFlags = StuckFlags
  { stuckFlagFilter :: !(Maybe ProjectFilter),
    stuckFlagThreshold :: !(Maybe Time)
  }
  deriving (Show, Eq)

data LogFlags = LogFlags
  { logFlagFilter :: !(Maybe EntryFilter),
    logFlagPeriodFlags :: !(Maybe Period),
    logFlagBlockFlags :: !(Maybe TimeBlock),
    logFlagHideArchive :: !(Maybe HideArchive)
  }
  deriving (Show, Eq, Generic)

newtype StatsFlags = StatsFlags
  { statsFlagPeriodFlags :: Maybe Period
  }
  deriving (Show, Eq, Generic)

newtype TagsFlags = TagsFlags
  { tagsFlagFilter :: Maybe EntryFilter
  }
  deriving (Show, Eq, Generic)

newtype Flags = Flags
  { flagReportFlags :: Report.Flags
  }
  deriving (Show, Eq, Generic)

emptyEnvironment :: Environment
emptyEnvironment =
  Environment
    { envReportEnvironment = Report.emptyEnvironment,
      envHideArchive = Nothing
    }

data Environment = Environment
  { envReportEnvironment :: !Report.Environment,
    envHideArchive :: !(Maybe HideArchive)
  }
  deriving (Show, Eq, Generic)

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
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance Validity Configuration

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> objectCodec .= confReportConf
        <*> optionalFieldOrNull "hide-archive" "Whether or not to consider the archive, by default" .= confHideArchive
        <*> optionalFieldOrNull preparedReportConfigurationKey "Prepared report config" .= confPreparedReportConfiguration
        <*> colourConfigurationTopLevelObjectCodec .= confColourConfiguration

preparedReportConfigurationKey :: Text
preparedReportConfigurationKey = "report"

data PreparedReportConfiguration = PreparedReportConfiguration
  { preparedReportConfAvailableReports :: !(Maybe (Map Text PreparedReport))
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec PreparedReportConfiguration)

instance Validity PreparedReportConfiguration

instance HasCodec PreparedReportConfiguration where
  codec =
    object "PreparedReportConfiguration" $
      PreparedReportConfiguration <$> optionalFieldOrNull "reports" "Custom reports" .= preparedReportConfAvailableReports

data Dispatch
  = DispatchEntry !EntrySettings
  | DispatchReport !ReportSettings
  | DispatchWork !WorkSettings
  | DispatchWaiting !WaitingSettings
  | DispatchNext !NextSettings
  | DispatchClock !ClockSettings
  | DispatchAgenda !AgendaSettings
  | DispatchProjects !ProjectsSettings
  | DispatchStuck !StuckSettings
  | DispatchLog !LogSettings
  | DispatchStats !StatsSettings
  | DispatchTags !TagsSettings
  deriving (Show, Eq, Generic)

data EntrySettings = EntrySettings
  { entrySetFilter :: !(Maybe EntryFilter),
    entrySetProjection :: !(NonEmpty Projection),
    entrySetSorter :: !(Maybe Sorter),
    entrySetHideArchive :: !HideArchive,
    entrySetOutputFormat :: !OutputFormat
  }
  deriving (Show, Eq, Generic)

data ReportSettings = ReportSettings
  { reportSetReportName :: !(Maybe Text),
    reportSetAvailableReports :: !(Map Text PreparedReport),
    reportSetOutputFormat :: !OutputFormat
  }
  deriving (Show, Eq, Generic)

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
  deriving (Show, Eq, Generic)

data WaitingSettings = WaitingSettings
  { waitingSetFilter :: !(Maybe EntryFilter),
    waitingSetHideArchive :: !HideArchive,
    waitingSetThreshold :: !Time
  }
  deriving (Show, Eq, Generic)

data NextSettings = NextSettings
  { nextSetFilter :: !(Maybe EntryFilter),
    nextSetHideArchive :: !HideArchive
  }
  deriving (Show, Eq, Generic)

data ClockSettings = ClockSettings
  { clockSetFilter :: !(Maybe EntryFilter),
    clockSetPeriod :: !Period,
    clockSetBlock :: !TimeBlock,
    clockSetOutputFormat :: !OutputFormat,
    clockSetClockFormat :: !ClockFormat,
    clockSetReportStyle :: !ClockReportStyle,
    clockSetHideArchive :: !HideArchive
  }
  deriving (Show, Eq, Generic)

data AgendaSettings = AgendaSettings
  { agendaSetFilter :: !(Maybe EntryFilter),
    agendaSetHistoricity :: !AgendaHistoricity,
    agendaSetBlock :: !TimeBlock,
    agendaSetHideArchive :: !HideArchive,
    agendaSetPeriod :: !Period
  }
  deriving (Show, Eq, Generic)

data ProjectsSettings = ProjectsSettings
  { projectsSetFilter :: !(Maybe ProjectFilter)
  }
  deriving (Show, Eq, Generic)

data StuckSettings = StuckSettings
  { stuckSetFilter :: !(Maybe ProjectFilter),
    stuckSetThreshold :: !Time
  }
  deriving (Show, Eq, Generic)

data LogSettings = LogSettings
  { logSetFilter :: !(Maybe EntryFilter),
    logSetPeriod :: !Period,
    logSetBlock :: !TimeBlock,
    logSetHideArchive :: !HideArchive
  }
  deriving (Show, Eq, Generic)

data StatsSettings = StatsSettings
  { statsSetPeriod :: !Period
  }
  deriving (Show, Eq, Generic)

data TagsSettings = TagsSettings
  { tagsSetFilter :: !(Maybe EntryFilter)
  }
  deriving (Show, Eq, Generic)

data OutputFormat
  = OutputPretty
  | OutputYaml
  | OutputJSON
  | OutputJSONPretty
  deriving (Show, Eq, Generic)

data Settings = Settings
  { settingDirectorySettings :: !DirectorySettings,
    settingColourSettings :: !ColourSettings
  }
  deriving (Show, Eq, Generic)
