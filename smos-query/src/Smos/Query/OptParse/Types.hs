{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Query.OptParse.Types
  ( module Smos.Report.Clock.Types,
    module Smos.Report.Agenda.Types,
    module Smos.Query.OptParse.Types,
    module Smos.Report.ShouldPrint,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Validity
import Data.Yaml as Yaml
import GHC.Generics (Generic)
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
import Text.Colour
import Text.Colour.Code
import Text.Colour.Layout
import YamlParse.Applicative

data Arguments
  = Arguments Command (Report.FlagsWithConfigFile Flags)
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
  { entryFlagFilter :: Maybe EntryFilterRel,
    entryFlagProjection :: Maybe (NonEmpty Projection),
    entryFlagSorter :: Maybe Sorter,
    entryFlagHideArchive :: Maybe HideArchive
  }
  deriving (Show, Eq)

data ReportFlags = ReportFlags
  { reportFlagReportName :: Maybe Text
  }
  deriving (Show, Eq)

data WorkFlags = WorkFlags
  { workFlagContext :: Maybe ContextName,
    workFlagTime :: Maybe Time,
    workFlagFilter :: Maybe EntryFilterRel,
    workFlagProjection :: Maybe (NonEmpty Projection),
    workFlagSorter :: Maybe Sorter,
    workFlagHideArchive :: Maybe HideArchive,
    workFlagWaitingThreshold :: Maybe Word,
    workFlagStuckThreshold :: Maybe Word
  }
  deriving (Show, Eq)

data WaitingFlags = WaitingFlags
  { waitingFlagFilter :: Maybe EntryFilterRel,
    waitingFlagHideArchive :: Maybe HideArchive,
    waitingFlagThreshold :: Maybe Word
  }
  deriving (Show, Eq)

data NextFlags = NextFlags
  { nextFlagFilter :: Maybe EntryFilterRel,
    nextFlagHideArchive :: Maybe HideArchive
  }
  deriving (Show, Eq)

data ClockFlags = ClockFlags
  { clockFlagFilter :: Maybe EntryFilterRel,
    clockFlagPeriodFlags :: Maybe Period,
    clockFlagBlockFlags :: Maybe TimeBlock,
    clockFlagOutputFormat :: Maybe OutputFormat,
    clockFlagClockFormat :: Maybe ClockFormatFlags,
    clockFlagReportStyle :: Maybe ClockReportStyle,
    clockFlagHideArchive :: Maybe HideArchive
  }
  deriving (Show, Eq)

data ClockFormatFlags
  = ClockFormatTemporalFlag (Maybe TemporalClockResolution)
  | ClockFormatDecimalFlag (Maybe DecimalClockResolution)
  deriving (Show, Eq)

data AgendaFlags = AgendaFlags
  { agendaFlagFilter :: Maybe EntryFilterRel,
    agendaFlagHistoricity :: Maybe AgendaHistoricity,
    agendaFlagBlock :: Maybe TimeBlock,
    agendaFlagHideArchive :: Maybe HideArchive,
    agendaFlagPeriod :: Maybe Period
  }
  deriving (Show, Eq)

data ProjectsFlags = ProjectsFlags
  { projectsFlagFilter :: Maybe ProjectFilter
  }
  deriving (Show, Eq)

data StuckFlags = StuckFlags
  { stuckFlagFilter :: Maybe ProjectFilter,
    stuckFlagThreshold :: Maybe Word
  }
  deriving (Show, Eq)

data LogFlags = LogFlags
  { logFlagFilter :: Maybe EntryFilterRel,
    logFlagPeriodFlags :: Maybe Period,
    logFlagBlockFlags :: Maybe TimeBlock,
    logFlagHideArchive :: Maybe HideArchive
  }
  deriving (Show, Eq, Generic)

newtype StatsFlags = StatsFlags
  { statsFlagPeriodFlags :: Maybe Period
  }
  deriving (Show, Eq, Generic)

newtype TagsFlags = TagsFlags
  { tagsFlagFilter :: Maybe EntryFilterRel
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
  { envReportEnvironment :: Report.Environment,
    envHideArchive :: Maybe HideArchive
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
  { confReportConf :: Report.Configuration,
    confHideArchive :: Maybe HideArchive,
    confPreparedReportConfiguration :: Maybe PreparedReportConfiguration,
    confColourConfiguration :: Maybe ColourConfiguration
  }
  deriving (Show, Eq, Generic)

instance Validity Configuration

instance ToJSON Configuration where
  toJSON Configuration {..} =
    object $
      Report.configurationToObject confReportConf
        ++ [ "hide-archive" .= confHideArchive,
             preparedReportConfigurationKey .= confPreparedReportConfiguration,
             colourConfigurationKey .= confColourConfiguration
           ]

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    (\reportConf (a, b, c) -> Configuration reportConf a b c)
      <$> yamlSchema
      <*> objectParser
        "Configuration"
        ( (,,) <$> optionalField "hide-archive" "Whether or not to consider the archive, by default"
            <*> optionalField preparedReportConfigurationKey "Prepared report config"
            <*> optionalField colourConfigurationKey "Colour config"
        )

preparedReportConfigurationKey :: Text
preparedReportConfigurationKey = "report"

colourConfigurationKey :: Text
colourConfigurationKey = "colour"

data PreparedReportConfiguration = PreparedReportConfiguration
  { preparedReportConfAvailableReports :: Maybe (Map Text PreparedReport)
  }
  deriving (Show, Eq, Generic)

instance Validity PreparedReportConfiguration

instance ToJSON PreparedReportConfiguration where
  toJSON PreparedReportConfiguration {..} = object ["reports" .= preparedReportConfAvailableReports]

instance FromJSON PreparedReportConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema PreparedReportConfiguration where
  yamlSchema = objectParser "PreparedReportConfiguration" $ PreparedReportConfiguration <$> optionalField "reports" "Custom reports"

data ColourConfiguration = ColourConfiguration
  { -- | How to background-colour tables
    --
    -- The first maybe is for whether this is defined in the configuration file.
    -- The second maybe is for whether any background colour should be used.
    colourConfigurationBackground :: Maybe TableBackgroundConfiguration
  }
  deriving (Show, Eq, Generic)

instance Validity ColourConfiguration

instance ToJSON ColourConfiguration where
  toJSON ColourConfiguration {..} =
    object
      [ "background" .= colourConfigurationBackground
      ]

instance FromJSON ColourConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema ColourConfiguration where
  yamlSchema =
    objectParser "ColourConfiguration" $
      ColourConfiguration
        <$> optionalField "background" "The table background colours"

data TableBackgroundConfiguration
  = UseTableBackground TableBackground
  | NoTableBackground
  deriving (Show, Eq, Generic)

instance Validity TableBackgroundConfiguration

instance ToJSON TableBackgroundConfiguration where
  toJSON = \case
    NoTableBackground -> Null
    UseTableBackground tb -> toJSON tb

instance FromJSON TableBackgroundConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema TableBackgroundConfiguration where
  yamlSchema =
    alternatives
      [ NoTableBackground <$ ParseNull,
        UseTableBackground <$> yamlSchema
      ]

instance ToJSON TableBackground where
  toJSON = \case
    SingleColour c -> toJSON c
    Bicolour e o -> object ["even" .= e, "odd" .= o]

instance FromJSON TableBackground where
  parseJSON = viaYamlSchema

instance YamlSchema TableBackground where
  yamlSchema =
    alternatives
      [ SingleColour <$> yamlSchema <?> "A single background colour",
        objectParser "Bicolour" $
          Bicolour
            <$> optionalField "even" "background for even-numbered table-rows (0-indexed)"
            <*> optionalField "odd" "background for odd-numbered table-rows"
      ]

instance ToJSON Colour where
  toJSON = \case
    Colour8 intensity colour8 ->
      toJSON $
        mconcat
          [ case intensity of
              Dull -> ("" :: Text)
              Bright -> "bright ",
            case colour8 of
              Black -> "black"
              Red -> "red"
              Green -> "green"
              Yellow -> "yellow"
              Blue -> "blue"
              Magenta -> "magenta"
              Cyan -> "cyan"
              White -> "white"
          ]
    Colour8Bit w8 -> toJSON w8
    Colour24Bit r g b -> object ["red" .= r, "green" .= g, "blue" .= b]

instance FromJSON Colour where
  parseJSON = viaYamlSchema

instance YamlSchema Colour where
  yamlSchema =
    alternatives
      [ eitherParser
          ( \t -> do
              let colourCase :: String -> Either String TerminalColour
                  colourCase = \case
                    "black" -> Right Black
                    "red" -> Right Red
                    "green" -> Right Green
                    "yellow" -> Right Yellow
                    "blue" -> Right Blue
                    "magenta" -> Right Magenta
                    "cyan" -> Right Cyan
                    "white" -> Right White
                    s -> Left $ "Unknown colour: " <> s
              case words t of
                [colourStr] -> Colour8 Dull <$> colourCase colourStr
                [intensityStr, colourStr] -> do
                  intensity <- case intensityStr of
                    "bright" -> Right Bright
                    "dull" -> Right Dull
                    _ -> Left $ "Unknown colour intensity: " <> intensityStr
                  Colour8 intensity <$> colourCase colourStr
                _ -> Left "Specify a terminal colour as two words, e. g. 'dull red' or 'bright blue'."
          )
          yamlSchema,
        Colour8Bit <$> yamlSchema
          <??> [ "Set this to a number between 0 and 255 that represents the colour that you want from the 8-bit colour schema.",
                 "See this overview on wikipedia for more information:",
                 "https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit"
               ],
        objectParser "Colour24Bit" $
          Colour24Bit
            <$> requiredField "red" "The red component, [0..255]"
            <*> requiredField "green" "The green component, [0..255]"
            <*> requiredField "blue" "The blue component, [0..255]"
      ]

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
  { entrySetFilter :: Maybe EntryFilterRel,
    entrySetProjection :: NonEmpty Projection,
    entrySetSorter :: Maybe Sorter,
    entrySetHideArchive :: HideArchive
  }
  deriving (Show, Eq, Generic)

data ReportSettings = ReportSettings
  { reportSetReportName :: Maybe Text,
    reportSetAvailableReports :: Map Text PreparedReport
  }
  deriving (Show, Eq, Generic)

data WorkSettings = WorkSettings
  { workSetContext :: !(Maybe ContextName),
    workSetContexts :: !(Map ContextName EntryFilterRel),
    workSetChecks :: !(Set EntryFilterRel),
    workSetTime :: !(Maybe Time),
    workSetTimeProperty :: !(Maybe PropertyName),
    workSetBaseFilter :: !(Maybe EntryFilterRel),
    workSetFilter :: !(Maybe EntryFilterRel),
    workSetProjection :: !(NonEmpty Projection),
    workSetSorter :: !(Maybe Sorter),
    workSetHideArchive :: !HideArchive,
    workSetWaitingThreshold :: !Word,
    workSetStuckThreshold :: !Word
  }
  deriving (Show, Eq, Generic)

data WaitingSettings = WaitingSettings
  { waitingSetFilter :: !(Maybe EntryFilterRel),
    waitingSetHideArchive :: !HideArchive,
    waitingSetThreshold :: !Word
  }
  deriving (Show, Eq, Generic)

data NextSettings = NextSettings
  { nextSetFilter :: Maybe EntryFilterRel,
    nextSetHideArchive :: HideArchive
  }
  deriving (Show, Eq, Generic)

data ClockSettings = ClockSettings
  { clockSetFilter :: Maybe EntryFilterRel,
    clockSetPeriod :: Period,
    clockSetBlock :: TimeBlock,
    clockSetOutputFormat :: OutputFormat,
    clockSetClockFormat :: ClockFormat,
    clockSetReportStyle :: ClockReportStyle,
    clockSetHideArchive :: HideArchive
  }
  deriving (Show, Eq, Generic)

data AgendaSettings = AgendaSettings
  { agendaSetFilter :: Maybe EntryFilterRel,
    agendaSetHistoricity :: AgendaHistoricity,
    agendaSetBlock :: TimeBlock,
    agendaSetHideArchive :: HideArchive,
    agendaSetPeriod :: Period
  }
  deriving (Show, Eq, Generic)

data ProjectsSettings = ProjectsSettings
  { projectsSetFilter :: Maybe ProjectFilter
  }
  deriving (Show, Eq, Generic)

data StuckSettings = StuckSettings
  { stuckSetFilter :: !(Maybe ProjectFilter),
    stuckSetThreshold :: !Word
  }
  deriving (Show, Eq, Generic)

data LogSettings = LogSettings
  { logSetFilter :: Maybe EntryFilterRel,
    logSetPeriod :: Period,
    logSetBlock :: TimeBlock,
    logSetHideArchive :: HideArchive
  }
  deriving (Show, Eq, Generic)

newtype StatsSettings = StatsSettings
  { statsSetPeriod :: Period
  }
  deriving (Show, Eq, Generic)

newtype TagsSettings = TagsSettings
  { tagsSetFilter :: Maybe EntryFilterRel
  }
  deriving (Show, Eq, Generic)

data OutputFormat
  = OutputPretty
  | OutputYaml
  | OutputJSON
  | OutputJSONPretty
  deriving (Show, Eq, Generic)

data Settings = Settings
  { settingDirectoryConfig :: !DirectoryConfig,
    settingColourSettings :: !ColourSettings
  }
  deriving (Show, Eq, Generic)

data ColourSettings = ColourSettings
  { colourSettingBackground :: TableBackgroundConfiguration
  }
  deriving (Show, Eq, Generic)
